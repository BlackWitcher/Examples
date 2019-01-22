unit sDBBtns;
{$I sDefs.inc}

interface

uses Windows, Controls, Classes, DB, DBCtrls, sButton, Dialogs, sDialogs,
  sSpeedButton, sBitBtn;

resourcestring
  rsDeleteRecordQuestion = 'Delete this record?';

type
  IsDBNavControl = interface
    procedure EditingChanged;
    procedure DataSetChanged;
    procedure ActiveChanged;
  end;

  TsDBNavBtnClick = procedure(var AllowAction: Boolean; Sender: TObject) of object;
  TsDBNavBtnClicked = procedure(Sender: TObject) of object;

  TsNavButtonDataLink = class;

  {$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsDBButton = class(TsButton, IsDBNavControl)
  private
    FDataLink: TsNavButtonDataLink;
    FBtnFunction: TNavigateBtn;
    FBeforeAction: TsDBNavBtnClick;
    FAfterAction: TsDBNavBtnClicked;
    FConfirmDelete: Boolean;
    function GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);
    procedure SetBtnFunction(const Value: TNavigateBtn);
  protected
    procedure DataSetChanged;
    procedure EditingChanged;
    procedure ActiveChanged;
  public
    procedure Click; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ButtonFunction : TNavigateBtn read FBtnFunction write SetBtnFunction;
    property ConfirmDelete : Boolean read FConfirmDelete write FConfirmDelete default True;
    property BeforeAction  : TsDBNavBtnClick read FBeforeAction write FBeforeAction;
    property AfterAction   : TsDBNavBtnClicked read FAfterAction write FAfterAction;
  end;

  TsDBSpeedButton = class(TsSpeedButton, IsDBNavControl)
  private
    FDataLink: TsNavButtonDataLink;
    FBtnFunction: TNavigateBtn;
    FBeforeAction: TsDBNavBtnClick;
    FAfterAction: TsDBNavBtnClicked;
    FConfirmDelete: Boolean;
    function GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);
    procedure SetBtnFunction(const Value: TNavigateBtn);
  protected
    procedure DataSetChanged;
    procedure EditingChanged;
    procedure ActiveChanged;
  public
    procedure Click; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ButtonFunction : TNavigateBtn read FBtnFunction write SetBtnFunction;
    property ConfirmDelete : Boolean read FConfirmDelete write FConfirmDelete default True;
    property BeforeAction  : TsDBNavBtnClick read FBeforeAction write FBeforeAction;
    property AfterAction   : TsDBNavBtnClicked read FAfterAction write FAfterAction;
  end;

  TsDBBitBtn = class(TsBitBtn, IsDBNavControl)
  private
    FDataLink: TsNavButtonDataLink;
    FBtnFunction: TNavigateBtn;
    FBeforeAction: TsDBNavBtnClick;
    FAfterAction: TsDBNavBtnClicked;
    FConfirmDelete: Boolean;
    function GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);
    procedure SetBtnFunction(const Value: TNavigateBtn);
  protected
    procedure DataSetChanged;
    procedure EditingChanged;
    procedure ActiveChanged;
  public
    procedure Click; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ButtonFunction : TNavigateBtn read FBtnFunction write SetBtnFunction;
    property ConfirmDelete : Boolean read FConfirmDelete write FConfirmDelete default True;
    property BeforeAction  : TsDBNavBtnClick read FBeforeAction write FBeforeAction;
    property AfterAction   : TsDBNavBtnClicked read FAfterAction write FAfterAction;
  end;

  TsNavButtonDataLink = class(TDataLink)
  private
    FNavigator: IsDBNavControl;
  protected
    procedure EditingChanged; override;
    procedure DataSetChanged; override;
    procedure ActiveChanged; override;
  public
    constructor Create(ANav: IsDBNavControl);
    destructor Destroy; override;
  end;

implementation

{ TsNavButtonDataLink }

procedure TsNavButtonDataLink.ActiveChanged;
begin
  if FNavigator <> nil then
    FNavigator.ActiveChanged;
end;

constructor TsNavButtonDataLink.Create(ANav: IsDBNavControl);
begin
  inherited Create;
  FNavigator := ANav;
  VisualControl := True;
end;

procedure TsNavButtonDataLink.DataSetChanged;
begin
  if FNavigator <> nil then
    FNavigator.DataSetChanged;
end;

destructor TsNavButtonDataLink.Destroy;
begin
  FNavigator := nil;
  inherited;
end;

procedure TsNavButtonDataLink.EditingChanged;
begin
  if FNavigator <> nil then
    FNavigator.EditingChanged;
end;

{ TsDBButton }

procedure TsDBButton.ActiveChanged;
begin
  if not FDataLink.Active then
    begin
      Enabled := False
    end
  else
    begin
      DataSetChanged;
      EditingChanged;
    end;
end;

procedure TsDBButton.Click;
var
  AllowAction: Boolean;
begin
  inherited;

  AllowAction := True;

  if not (csDesigning in ComponentState) and Assigned(FBeforeAction) then
    FBeforeAction(AllowAction, Self);

  if (DataSource <> nil) and (DataSource.State <> dsInactive) and (AllowAction) then
    begin
      case FBtnFunction of
        nbInsert:
          begin
            DataSource.DataSet.Insert;
          end;
        nbEdit:
          begin
            DataSource.DataSet.Edit;
          end;
        nbPost:
          begin
            DataSource.DataSet.Post;
          end;
        nbCancel:
          begin
            DataSource.DataSet.Cancel;
          end;
        nbDelete:
          begin
            if not FConfirmDelete or (sMessageDlg(rsDeleteRecordQuestion, mtConfirmation, mbOKCancel, 0) <> idCancel) then
              DataSource.DataSet.Delete;
          end;
        nbRefresh:
          begin
            DataSource.DataSet.Refresh;
          end;
        nbFirst:
          begin
            DataSource.DataSet.First;
          end;
        nbPrior:
          begin
            DataSource.DataSet.Prior;
          end;
        nbNext:
          begin
            DataSource.DataSet.Next;
          end;
        nbLast:
          begin
            DataSource.DataSet.Last;
          end;
      end;//case
    end;

  if Assigned(FAfterAction) then FAfterAction(Self);
end;

constructor TsDBButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TsNavButtonDataLink.Create(Self);
  FConfirmDelete := True;
  SetBtnFunction(nbFirst);
  Enabled := False;
end;

procedure TsDBButton.DataSetChanged;
var
  UpEnable, DnEnable: Boolean;
begin
  if not (csLoading in ComponentState) then
    begin
      UpEnable := FDataLink.Active and not FDataLink.DataSet.Bof;
      DnEnable := FDataLink.Active and not FDataLink.DataSet.Eof;
      case FBtnFunction of
        nbFirst : Enabled := UpEnable;
        nbPrior : Enabled := UpEnable;
        nbNext  : Enabled := DnEnable;
        nbLast  : Enabled := DnEnable;
        nbDelete: Enabled := FDataLink.Active and FDataLink.DataSet.CanModify and
                              not (FDataLink.DataSet.BOF and FDataLink.DataSet.EOF);
//        nbInsert: Enabled := FDataLink.Active and FDataLink.DataSet.CanModify;
      end;
    end;
end;

destructor TsDBButton.Destroy;
begin
  FDataLink.Free;
  inherited Destroy;
end;

procedure TsDBButton.EditingChanged;
var
  CanModify: Boolean;
begin
  if not (csLoading in ComponentState) then
    begin
      CanModify := FDataLink.Active and FDataLink.DataSet.CanModify;

      case FBtnFunction of
        nbInsert  : Enabled := CanModify;
        nbEdit    : Enabled := CanModify and not FDataLink.Editing;
        nbPost    : Enabled := CanModify and FDataLink.Editing;
        nbCancel  : Enabled := CanModify and FDataLink.Editing;
        nbRefresh : Enabled := CanModify;
      end;//case
    end;
end;

function TsDBButton.GetDataSource: TDataSource;
begin
  if Assigned(FDataLink) and Assigned(FDataLink.DataSource) then
    Result := FDataLink.DataSource
  else
    Result := nil;
end;

procedure TsDBButton.SetBtnFunction(const Value: TNavigateBtn);
begin
  if Value <> FBtnFunction then
    FBtnFunction := Value;
end;

procedure TsDBButton.SetDataSource(const Value: TDataSource);
begin
  if Value <> FDataLink.DataSource then
    FDataLink.DataSource := Value;
  if not (csLoading in ComponentState) then
    ActiveChanged;

  if Value <> nil then
    Value.FreeNotification(Self);
end;

{ TsDBSpeedButton }

procedure TsDBSpeedButton.ActiveChanged;
begin
  if not FDataLink.Active then
    begin
      Enabled := False
    end
  else
    begin
      DataSetChanged;
      EditingChanged;
    end;
end;

procedure TsDBSpeedButton.Click;
var
  AllowAction: Boolean;
begin
  inherited;

  AllowAction := True;

  if not (csDesigning in ComponentState) and Assigned(FBeforeAction) then
    FBeforeAction(AllowAction, Self);

  if (DataSource <> nil) and (DataSource.State <> dsInactive) and (AllowAction) then
    begin
      case FBtnFunction of
        nbInsert:
          begin
            DataSource.DataSet.Insert;
          end;
        nbEdit:
          begin
            DataSource.DataSet.Edit;
          end;
        nbPost:
          begin
            DataSource.DataSet.Post;
          end;
        nbCancel:
          begin
            DataSource.DataSet.Cancel;
          end;
        nbDelete:
          begin
            if not FConfirmDelete or (sMessageDlg(rsDeleteRecordQuestion, mtConfirmation, mbOKCancel, 0) <> idCancel) then
              DataSource.DataSet.Delete;
          end;
        nbRefresh:
          begin
            DataSource.DataSet.Refresh;
          end;
        nbFirst:
          begin
            DataSource.DataSet.First;
          end;
        nbPrior:
          begin
            DataSource.DataSet.Prior;
          end;
        nbNext:
          begin
            DataSource.DataSet.Next;
          end;
        nbLast:
          begin
            DataSource.DataSet.Last;
          end;
      end;//case
    end;

  if Assigned(FAfterAction) then FAfterAction(Self);
end;

constructor TsDBSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TsNavButtonDataLink.Create(Self);
  FConfirmDelete := True;
  SetBtnFunction(nbFirst);
  Enabled := False;
end;

procedure TsDBSpeedButton.DataSetChanged;
var
  UpEnable, DnEnable: Boolean;
begin
  if not (csLoading in ComponentState) then
    begin
      UpEnable := FDataLink.Active and not FDataLink.DataSet.Bof;
      DnEnable := FDataLink.Active and not FDataLink.DataSet.Eof;
      case FBtnFunction of
        nbFirst : Enabled := UpEnable;
        nbPrior : Enabled := UpEnable;
        nbNext  : Enabled := DnEnable;
        nbLast  : Enabled := DnEnable;
        nbDelete: Enabled := FDataLink.Active and FDataLink.DataSet.CanModify and
                              not (FDataLink.DataSet.BOF and FDataLink.DataSet.EOF);
      end;
    end;
end;

destructor TsDBSpeedButton.Destroy;
begin
  FDataLink.Free;
  inherited Destroy;
end;

procedure TsDBSpeedButton.EditingChanged;
var
  CanModify: Boolean;
begin
  if not (csLoading in ComponentState) then
    begin
      CanModify := FDataLink.Active and FDataLink.DataSet.CanModify;

      case FBtnFunction of
        nbInsert  : Enabled := CanModify;
        nbEdit    : Enabled := CanModify and not FDataLink.Editing;
        nbPost    : Enabled := CanModify and FDataLink.Editing;
        nbCancel  : Enabled := CanModify and FDataLink.Editing;
        nbRefresh : Enabled := CanModify;
      end;//case
    end;
end;

function TsDBSpeedButton.GetDataSource: TDataSource;
begin
  if Assigned(FDataLink) and Assigned(FDataLink.DataSource) then
    Result := FDataLink.DataSource
  else
    Result := nil;
end;

procedure TsDBSpeedButton.SetBtnFunction(const Value: TNavigateBtn);
begin
  if Value <> FBtnFunction then
    FBtnFunction := Value;
end;

procedure TsDBSpeedButton.SetDataSource(const Value: TDataSource);
begin
  if Value <> FDataLink.DataSource then
    FDataLink.DataSource := Value;
  if not (csLoading in ComponentState) then
    ActiveChanged;

  if Value <> nil then
    Value.FreeNotification(Self);
end;

{ TsDBBitBtn }

procedure TsDBBitBtn.ActiveChanged;
begin
  if not FDataLink.Active then
    begin
      Enabled := False
    end
  else
    begin
      DataSetChanged;
      EditingChanged;
    end;
end;

procedure TsDBBitBtn.Click;
var
  AllowAction: Boolean;
begin
  inherited;

  AllowAction := True;

  if not (csDesigning in ComponentState) and Assigned(FBeforeAction) then
    FBeforeAction(AllowAction, Self);

  if (DataSource <> nil) and (DataSource.State <> dsInactive) and (AllowAction) then
    begin
      case FBtnFunction of
        nbInsert:
          begin
            DataSource.DataSet.Insert;
          end;
        nbEdit:
          begin
            DataSource.DataSet.Edit;
          end;
        nbPost:
          begin
            DataSource.DataSet.Post;
          end;
        nbCancel:
          begin
            DataSource.DataSet.Cancel;
          end;
        nbDelete:
          begin
            if not FConfirmDelete or (sMessageDlg(rsDeleteRecordQuestion, mtConfirmation, mbOKCancel, 0) <> idCancel) then
              DataSource.DataSet.Delete;
          end;
        nbRefresh:
          begin
            DataSource.DataSet.Refresh;
          end;
        nbFirst:
          begin
            DataSource.DataSet.First;
          end;
        nbPrior:
          begin
            DataSource.DataSet.Prior;
          end;
        nbNext:
          begin
            DataSource.DataSet.Next;
          end;
        nbLast:
          begin
            DataSource.DataSet.Last;
          end;
      end;//case
    end;

  if Assigned(FAfterAction) then FAfterAction(Self);
end;

constructor TsDBBitBtn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TsNavButtonDataLink.Create(Self);
  FConfirmDelete := True;
  SetBtnFunction(nbFirst);
  Enabled := False;
end;

procedure TsDBBitBtn.DataSetChanged;
var
  UpEnable, DnEnable: Boolean;
begin
  if not (csLoading in ComponentState) then
    begin
      UpEnable := FDataLink.Active and not FDataLink.DataSet.Bof;
      DnEnable := FDataLink.Active and not FDataLink.DataSet.Eof;
      case FBtnFunction of
        nbFirst : Enabled := UpEnable;
        nbPrior : Enabled := UpEnable;
        nbNext  : Enabled := DnEnable;
        nbLast  : Enabled := DnEnable;
        nbDelete: Enabled := FDataLink.Active and FDataLink.DataSet.CanModify and
                              not (FDataLink.DataSet.BOF and FDataLink.DataSet.EOF);
      end;
    end;
end;

destructor TsDBBitBtn.Destroy;
begin
  FDataLink.Free;
  inherited Destroy;
end;

procedure TsDBBitBtn.EditingChanged;
var
  CanModify: Boolean;
begin
  if not (csLoading in ComponentState) then
    begin
      CanModify := FDataLink.Active and FDataLink.DataSet.CanModify;

      case FBtnFunction of
        nbInsert  : Enabled := CanModify;
        nbEdit    : Enabled := CanModify and not FDataLink.Editing;
        nbPost    : Enabled := CanModify and FDataLink.Editing;
        nbCancel  : Enabled := CanModify and FDataLink.Editing;
        nbRefresh : Enabled := CanModify;
      end;//case
    end;
end;

function TsDBBitBtn.GetDataSource: TDataSource;
begin
  if Assigned(FDataLink) and Assigned(FDataLink.DataSource) then
    Result := FDataLink.DataSource
  else
    Result := nil;
end;

procedure TsDBBitBtn.SetBtnFunction(const Value: TNavigateBtn);
begin
  if Value <> FBtnFunction then
    FBtnFunction := Value;
end;

procedure TsDBBitBtn.SetDataSource(const Value: TDataSource);
begin
  if Value <> FDataLink.DataSource then
    FDataLink.DataSource := Value;
  if not (csLoading in ComponentState) then
    ActiveChanged;

  if Value <> nil then
    Value.FreeNotification(Self);
end;

end.
