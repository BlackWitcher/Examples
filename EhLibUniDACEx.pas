unit EhLibUniDACEx;

interface

uses
  Windows, Classes, SysUtils, EhLibVCL, DBGridEh, DbUtilsEh, Db, Uni, ToolCtrlsEh;

type
  TUniDacDatasetFeaturesEh = class(TSQLDatasetFeaturesEh)
  public
//    function GetLikeWildcardForSeveralCharacters: String; override;
    procedure ApplyFilter(Sender: TObject; DataSet: TDataSet; IsReopen: Boolean); override;
    procedure ApplySorting(Sender: TObject; DataSet: TDataSet; IsReopen: Boolean); override;
    procedure FillSTFilterListDataValues(AGrid: TCustomDBGridEh; Column: TColumnEh; Items: TStrings); override;
//    procedure FillFieldUniqueValues(Field: TField; Items: TStrings); override;
  end;

  function WrapFieldName(const FieldName: String): String;


implementation

function StrCmpLogicalW(psz1, psz2: PWideChar): Integer; stdcall; external 'shlwapi.dll';

function StrCmpLogical(const s1, s2: string): Integer;
begin
  Result := StrCmpLogicalW(PChar(s1), PChar(s2));
end;

function WrapFieldName(const FieldName: String): String;
var
  i: Integer;
begin
  for i := 1 to Length(FieldName) do
  begin
    if CharInSetEh(FieldName[i], [' ', ',', ';', '''']) then
    begin
      Result := '[' + FieldName + ']';
      Exit;
    end;
  end;
  Result := FieldName;
end;

procedure TUniDacDatasetFeaturesEh.ApplyFilter(Sender: TObject; DataSet: TDataSet;
  IsReopen: Boolean);
begin
  if TDBGridEh(Sender).STFilter.Local then
  begin
    TDBGridEh(Sender).DataSource.DataSet.Filter :=
      GetExpressionAsFilterString(TDBGridEh(Sender),
        GetOneExpressionAsLocalFilterString, nil);
    TDBGridEh(Sender).DataSource.DataSet.Filtered := True;
  end else
    TCustomUniDataSet(DataSet).FilterSQL := GetExpressionAsFilterString(TDBGridEh(Sender),
        GetOneExpressionAsLocalFilterString, nil);
end;

procedure TUniDacDatasetFeaturesEh.ApplySorting(Sender: TObject; DataSet: TDataSet;
  IsReopen: Boolean);
var
  i: Integer;
  sOrderFields: String;
begin
  if Sender is TCustomDBGridEh then
    begin
      for i := 0 to TCustomDBGridEh(Sender).SortMarkedColumns.Count - 1 do
        begin
          if TCustomDBGridEh(Sender).SortMarkedColumns[i].LookupParams.LookupActive then
            begin
              //Lookup field
              sOrderFields := sOrderFields + TCustomDBGridEh(Sender).SortMarkedColumns[i].LookupParams.KeyFieldNames;
            end
          else
            begin
              //Regular field
              sOrderFields := sOrderFields + TCustomDBGridEh(Sender).SortMarkedColumns[i].FieldName;
            end;
          if TCustomDBGridEh(Sender).SortMarkedColumns[i].Title.SortMarker = smUpEh then
            sOrderFields := sOrderFields + ' DESC';
          sOrderFields := sOrderFields + ';';
        end;
      Delete(sOrderFields,Length(sOrderFields),1);
    end;

  if TCustomDBGridEh(Sender).SortLocal then
    TCustomUniDataSet(TCustomDBGridEh(Sender).DataSource.DataSet).IndexFieldNames := sOrderFields
  else if DataSet is TUniTable then
    TUniTable(DataSet).OrderFields := sOrderFields
  else
    inherited ApplySorting(Sender, DataSet, IsReopen);
  if TCustomDBGridEh(Sender).DataSource is TDataSource then
    begin
      DataSet.Resync([rmExact]);
    end;
end;

//procedure TUniDacDatasetFeaturesEh.FillFieldUniqueValues(Field: TField;
//  Items: TStrings);
//var
//  ud: TCustomUniDataSet;
//  BM : TBookmark;
//begin
//  if (Field <> nil) and (Field.DataSet <> nil) and Field.DataSet.Active and
//     (Field.DataSet is TCustomUniDataSet)
//  then
//  begin
//    Items.Clear;
//    Items.BeginUpdate;
//    ud := TCustomUniDataSet(Field.DataSet);
//    BM := ud.GetBookmark;
//    ud.DisableControls;
//    ud.First;
//    try
//      while not ud.Eof do
//        begin
//          Items.Add(ud.FieldByName(Field.FieldName).AsString);
//          ud.Next;
//        end;
//    finally
//      ud.GotoBookmark(BM);
//      Items.EndUpdate;
//      ud.FreeBookmark(BM);
//      ud.EnableControls;
//    end;
//  end;
//end;

{$REGION 'Comparators'}
function CompareInt(List: TStringList; Index1, Index2: Integer): Integer;
var
  d1, d2: Int64;
  r1, r2: Boolean;

  function IsInt(AString : string; var AInteger : Int64): Boolean;
  var
    Code: Integer;
  begin
    Val(AString, AInteger, Code);
    Result := (Code = 0);
  end;
begin
  //Comparer for integer field values
  r1 :=  IsInt(List[Index1], d1);
  r2 :=  IsInt(List[Index2], d2);
  Result := ord(r1 or r2);
  if Result <> 0 then
    begin
      if d1 < d2 then
        Result := -1
      else if d1 > d2 then
        Result := 1
      else
       Result := 0;
    end
  else
    Result := lstrcmp(PChar(List[Index1]), PChar(List[Index2]));
end;

function CompareString(List: TStringList; Index1, Index2: Integer): Integer;
begin
  //This string comparer use WinAPI comparer that Explorer use to sort data.
  //You can rewrite this method witout API if it's needed.
  Result := StrCmpLogical(List[Index1], List[Index2]);
end;

function CompareDateTime(List: TStringList; Index1, Index2: Integer): Integer;
var
   d1, d2: TDateTime;
begin
  //DataTime comparer. Uses current FormatSettings. If your data/time string
  //representations are different it, you must re-define FormatSettings before
  //using this function. Or you always can write own comparer for your own
  //date/time representation

//   d1 := StrToDateTime(List[Index1]);
//   d2 := StrToDateTime(List[Index2]);
  d1 := -1; d2 := -1;
  TryStrToDateTime(List[Index1], d1);
  TryStrToDateTime(List[Index2], d2);
  if d1 < d2 then
    Result := -1
  else if d1 > d2 then Result := 1
  else
    Result := 0;
end;

function CompareFloat(List: TStringList; Index1, Index2: Integer): Integer;
var
  Values: array [0..1] of
    record
      value: extended;
      valid: Boolean
    end;
begin
  //Float comparer
  Values[0].valid := TryStrToFloat(List[Index1], Values[0].Value);
  Values[1].valid := TryStrToFloat(List[Index2], Values[1].Value);
  if Values[0].valid then
    if Values[1].valid then begin
      if Values[0].Value > Values[1].Value then
        Result := 1
      else if Values[0].Value < Values[1].Value then
        Result := -1
      else
        Result := 0
    end
    else
      Result := 1
  else
    if Values[1].valid then
      Result := -1
    else
      Result := AnsiCompareText(List[Index1], List[Index2]);
end;

function CompareBoolean(List: TStringList; Index1, Index2: Integer): Integer;
var
  b1, b2 : Boolean;
begin
  //Boolean comparer
  b1 := False; b2 := False;
  TryStrToBool(List[Index1], b1);
  TryStrToBool(List[Index2], b2);
  Result := ord(b1) - ord(b2);
end;
{$ENDREGION}

procedure TUniDacDatasetFeaturesEh.FillSTFilterListDataValues(
  AGrid: TCustomDBGridEh; Column: TColumnEh; Items: TStrings);
var
  ValuesList: TStringList;
  dsfv:  TDatasetFieldValueListEh;
  i, k : Integer;
begin
  if not (AGrid.DataSource.DataSet is TDataSet) then Exit;

  //Field values list: contains resulting unique data values from dataset column
  ValuesList := TStringList.Create;
  ValuesList.Duplicates := dupIgnore;
  ValuesList.CaseSensitive := not Column.CaseInsensitiveTextSearch;
  ValuesList.Sorted := False;

  dsfv := TDatasetFieldValueListEh.Create;
  dsfv.CaseSensitive := not Column.CaseInsensitiveTextSearch;

  try
    if Column.LookupParams.LookupActive then
      begin
        //There is a lookup field
        if AGrid.STFilter.Location = stflInTitleFilterEh then Items.Clear;

        dsfv.FieldName := Column.LookupParams.LookupDisplayFieldName;
        dsfv.DataSet := Column.LookupParams.LookupDataSet;
        ValuesList.AddStrings(dsfv.Values);
        //Check property DropDownSpecRow
        if Column.DropDownSpecRow.Visible then
          Items.AddObject(Column.DropDownSpecRow.CellText[0], nil);
      end
    else
      begin
        //There is regular field: get all unique field values to TStringList object
        if (Column.STFilter.ListSource = nil) and (not Column.Field.DataSet.IsEmpty) then
          begin
            if AGrid.STFilter.Location = stflInTitleFilterEh then Items.Clear;

            dsfv.FieldName := Column.FieldName;
            dsfv.DataSet := Column.Field.DataSet;
            ValuesList.AddStrings(dsfv.Values);
            //We need to sort our data values in natural order. You can change
            //compare procedures as you needed.
            case Column.Field.DataType of
              ftUnknown, ftString, ftMemo, ftFmtMemo, ftFixedChar, ftWideString,
              ftGuid, ftFixedWideChar, ftWideMemo :
                ValuesList.CustomSort(CompareString);

              ftSmallint, ftInteger, ftWord, ftAutoInc, ftLargeint, ftLongWord,
              ftShortint, ftByte :
                ValuesList.CustomSort(CompareInt);

              ftSingle, ftFloat, ftCurrency, ftBCD, ftExtended:
                ValuesList.CustomSort(CompareFloat);

              ftBoolean:
                ValuesList.CustomSort(CompareBoolean);

              ftDate, ftTime, ftDateTime, ftTimeStamp:
                begin
                  ValuesList.CustomSort(CompareDateTime);
                end;
            else
              ValuesList.Sort;
            end;
            //For regular field we neet to add comand items manually:
            if AGrid.STFilter.Location = stflUnderTitleFilterEh then
              AGrid.Center.DefaultFillSTFilterListCommandValues(AGrid, Column, Items);
          end;
      end;

    //Add our field values and command menu items to filter menu items
    if ValuesList.Count > 0 then
      begin
        Items.AddStrings(ValuesList);
      end
    else
      begin
        inherited FillSTFilterListDataValues(AGrid, Column, Items);
      end;

    //If KeyList/PickList used we must make replacement filter items with items from PickList
    if (Column.KeyList.Count > 0) and (Column.PickList.Count > 0) then
      begin
        for i := 0 to Items.Count - 1 do
        begin
          k := Column.KeyList.IndexOf(Items[i]);
          if k >= 0 then
            Items[i] := Column.PickList[k];
        end;
      end;
  finally
    FreeAndNil(ValuesList);
    FreeAndNil(dsfv);
  end;
end;

initialization
  RegisterDatasetFeaturesEh(TUniDacDatasetFeaturesEh, TUniQuery);
  RegisterDatasetFeaturesEh(TUniDacDatasetFeaturesEh, TUniTable);

end.
