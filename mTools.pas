{*******************************************************}
{                                                       }
{       Набор вспомогательных классов, функций и утилит }
{                                                       }
{       Copyright (C) 2018-2019 Witcher                 }
{                                                       }
{*******************************************************}

unit mTools;

interface

uses
  Windows, Classes, SysUtils, Dialogs, Messages, StdCtrls, Graphics, TypInfo,
  synacode, ShellAPI, Controls;

const
  WM_AFTER_SHOW = WM_USER + 300; // Сообщения для событий при создании событий AfterCreate и AfterShow формы
  WM_AFTER_CREATE = WM_USER + 301;

type
  TextHintColorHelper = class helper for TCustomEdit
  private
    procedure SetTextHintColor(const Value: TColor);
    function GetTextHintColor: TColor;
    procedure fixWndProc(var aMessage: TMessage);
  published
    property TextHintColor : TColor read GetTextHintColor write SetTextHintColor;
  end;

  TEnumConverter = class
  public
    class function EnumToInt<T>(const EnumValue: T): Integer;
    class function EnumToString<T>(EnumValue: T): string;
  end;

  //Класс для реализации анонимных методов как обработчиков событий
  //См. AnonProc2NotifyEvent 
  TNotifyEventWrapper = class(TComponent)
  private
    FProc: TProc<TObject>;
  public
    constructor Create(Owner: TComponent; Proc: TProc<TObject>); reintroduce;
  published
    procedure Event(Sender: TObject);
  end;

//Вытаскивает окно с хэндлом hwnd на передний план
function ForceForegroundWindow(hwnd: THandle): Boolean;

//Создаёт мьютекс для определения единственного экземпляра программы
function IsSingleInstance(MutexName : string; KeepMutex : boolean = true): boolean;

//Путь к приложению
function GetAppPath : string;

//Тест на 64 битную систему
function Is64BitOS: Boolean;

//Обрезка строки с многоточием
function StrMaxLen(const S: string; MaxLen: integer): string;

//Удаяет указанные символы из строки
function StripChars(const AStr: String; const RemoveChars: TSysCharSet): string;

//Получает размер файла без его загрузки в память
function FileSize(const aFilename: String): Int64;

//Извлекает из строки все цифры
function ExtractNumbersFromString(Source: string): string;

//Получение номера версии файла из его ресурсов
function GetEXEVersion(filename: string = ''; const Fmt: string = '%d.%d.%d.%d'): string;

//Реализация лямбд (анонимных методов) как обработчиков событий типа TNotifyEvent
//Например, так:
//  LabelURL.OnClick := AnonProc2NotifyEvent(LabelURL,
//    procedure(Sender: TObject)
//    begin
//      mTools.OpenURL(LabelURL.Caption);
//    end
//  );

function AnonProc2NotifyEvent(Owner: TComponent; Proc: TProc<TObject>): TNotifyEvent;

//Открывает указанный URL в браузере
procedure OpenURL(const URL : string);

//Обходит все дочерние компоненты контрола Parent и выполняет для них процедуру
//Visit.
//Пример использования:
//
//WalkChildren(
//  PageControl1,
//  procedure(Child: TControl)
//  begin
//    Memo1.Lines.Add(Child.Name);
//  end);
//             или
//for i := 0 to PageControl1.PageCount-1 do
//  WalkChildren(
//    PageControl1.Pages[i],
//    procedure(Child: TControl)
//    begin
//      Memo1.Lines.Add(Child.Name);
//    end
//  );
procedure WalkChildren(Parent: TWinControl; Visit: TProc<TControl>);

implementation

procedure WalkChildren(Parent: TWinControl; Visit: TProc<TControl>);
var
  i: Integer;
  Child: TControl;
begin
  for i := 0 to Parent.ControlCount-1 do
  begin
    Child := Parent.Controls[i];
    Visit(Child);
    if Child is TWinControl then
      WalkChildren(TWinControl(Child), Visit);
  end;
end;

procedure OpenURL(const URL : string);
var
  s : string;
begin
  s := EncodeURL(URL);
  ShellExecute(0, 'open', PChar(s), nil, nil, SW_SHOWNORMAL);
end;

function GetEXEVersion(filename: string = ''; const Fmt: string = '%d.%d.%d.%d'): string;
var
  iBufferSize: DWORD;
  iDummy: DWORD;
  pBuffer: Pointer;
  pFileInfo: Pointer;
  iVer: array[1..4] of word;
begin
  if filename = '' then
    filename := ParamStr(0);
  Result := '';

  iBufferSize := GetFileVersionInfoSize(PChar(filename), iDummy);
  if (iBufferSize > 0) then
  begin
    GetMem(pBuffer, iBufferSize);
    try
      GetFileVersionInfo(PChar(filename), 0, iBufferSize, pBuffer);
      VerQueryValue(pBuffer, '\', pFileInfo, iDummy);
      iVer[1] := HiWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionMS);
      iVer[2] := LoWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionMS);
      iVer[3] := HiWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionLS);
      iVer[4] := LoWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionLS);
    finally
      FreeMem(pBuffer);
    end;
    Result := Format(Fmt, [iVer[1], iVer[2], iVer[3], iVer[4]]);
  end;
end;

function ExtractNumbersFromString(Source: string): string;
var
  c : Char;
begin
  Result := EmptyStr;
  for c in Source do
    if CharInSet(c, ['0'..'9']) then Result := Result + c;
end;

function FileSize(const aFilename: String): Int64;
var
  info: TWin32FileAttributeData;
begin
  result := -1;
  if NOT GetFileAttributesEx(PWideChar(aFileName), GetFileExInfoStandard, @info) then EXIT;
  result := Int64(info.nFileSizeLow) or Int64(info.nFileSizeHigh shl 32);
end;

function StrMaxLen(const S: string; MaxLen: integer): string;
var
  i: Integer;
begin
  result := S;
  if Length(result) <= MaxLen then Exit;
  SetLength(result, MaxLen);
  for i := MaxLen downto MaxLen - 2 do
    result[i] := '.';
end;

type
  WinIsWow64 = function( Handle: THandle; var Iret: BOOL ): Windows.BOOL; stdcall;

function Is64BitOS: Boolean;
var
  HandleTo64BitsProcess: WinIsWow64;
  Iret                 : Windows.BOOL;
begin
  Result := False;
  HandleTo64BitsProcess := GetProcAddress(GetModuleHandle('kernel32.dll'), 'IsWow64Process');
  if Assigned(HandleTo64BitsProcess) then
  begin
    if not HandleTo64BitsProcess(GetCurrentProcess, Iret) then
      Raise Exception.Create('Invalid handle');
    Result := Iret;
  end;
end;

//Вытаскивает окно с хэндлом hwnd на передний план
function ForceForegroundWindow(hwnd: THandle): Boolean;
const
  SPI_GETFOREGROUNDLOCKTIMEOUT = $2000;
  SPI_SETFOREGROUNDLOCKTIMEOUT = $2001;
var
  ForegroundThreadID: DWORD;
  ThisThreadID: DWORD;
  timeout: DWORD;
begin
  if IsIconic(hwnd) then ShowWindow(hwnd, SW_RESTORE);

  if GetForegroundWindow = hwnd then Result := True
  else
  begin
    if ((Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion > 4)) or
        ((Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and
        ((Win32MajorVersion > 4) or ((Win32MajorVersion = 4) and
        (Win32MinorVersion > 0)))) then
      begin
        Result := False;
        ForegroundThreadID := GetWindowThreadProcessID(GetForegroundWindow, nil);
        ThisThreadID := GetWindowThreadPRocessId(hwnd, nil);
        if AttachThreadInput(ThisThreadID, ForegroundThreadID, True) then
          begin
            BringWindowToTop(hwnd); // IE 5.5 related hack
            SetForegroundWindow(hwnd);
            AttachThreadInput(ThisThreadID, ForegroundThreadID, False);
            Result := (GetForegroundWindow = hwnd);
          end;
        if not Result then
          begin
            SystemParametersInfo(SPI_GETFOREGROUNDLOCKTIMEOUT, 0, @timeout, 0);
            SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, TObject(0),
              SPIF_SENDCHANGE);
            BringWindowToTop(hwnd); // IE 5.5 related hack
            SetForegroundWindow(hWnd);
            SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, TObject(timeout), SPIF_SENDCHANGE);
          end;
      end
    else
      begin
        BringWindowToTop(hwnd); // IE 5.5 related hack
        SetForegroundWindow(hwnd);
      end;

    Result := (GetForegroundWindow = hwnd);
  end;
end;

//Создаёт мьютекс для определения единственного экземпляра программы
function IsSingleInstance(MutexName : string; KeepMutex : boolean = true): boolean;
const
    MUTEX_GLOBAL = 'Global\'; //Префикс для создания глобального мьютекса, видимого во всех сессиях и для разных пользователей (актуально для служб).
    MUTEX_SUFFIX = '_1755ADFD-256F-4537-9937-99094644BF87}'; //Суффикс для мьютекса

var MutexHandle : THandle;
    SecurityDesc: TSecurityDescriptor;
    SecurityAttr: TSecurityAttributes;
    ErrCode : integer;
begin
  //  По умолчанию (lpMutexAttributes = nil) создаётся мьютекс доступный только
  //  пользователю, запустившему процесс. Но нам нужен мьютекс, видимый всем
  //  юзерам, и потому поиск мьютекса будем делать по всем юзерам и сессиям.
  //  Что бы это сделать, нужно дескриптор безопасности DACL задать как null.
  InitializeSecurityDescriptor(@SecurityDesc, SECURITY_DESCRIPTOR_REVISION);
  SetSecurityDescriptorDacl(@SecurityDesc, True, nil, False);
  SecurityAttr.nLength:=SizeOf(SecurityAttr);
  SecurityAttr.lpSecurityDescriptor:=@SecurityDesc;
  SecurityAttr.bInheritHandle:=False;

  //  Создаём глобальный мьютекс
  MutexHandle := CreateMutex(@SecurityAttr, True, PChar(MUTEX_GLOBAL + MutexName + MUTEX_SUFFIX));
  ErrCode := GetLastError;

  //  Если что-то пошло не так, то возвращаемое значение будет равно нулю.
  //  Если мьютекс с таким именем уже есть, то функция  GetLastError вернёт
  //  ошибку ERROR_ALREADY_EXISTS.
  if {(MutexHandle = 0) or }(ErrCode = ERROR_ALREADY_EXISTS) then
    begin
      Result := false;
      CloseHandle(MutexHandle);
    end
  else
    begin
      //  Мьютекс ранее не был создан, что значит, что жанный экземпляр приложения - первый
      Result := true;

      if not KeepMutex then
         CloseHandle(MutexHandle);
    end;

  //  Не закрываем хэндл мьютекса в течении жизни приложения. Система сама его
  //  закроет автоматически после завершения приложения.
end;

function GetAppPath : string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
end;

function StripChars(const AStr: String; const RemoveChars: TSysCharSet): string;
var
  lBuilder: TStringBuilder;
  I: Integer;
begin
  //Чистит строку от указанных символов
  lBuilder := TStringBuilder.Create;
  try
    for I := 1 to Length(AStr) do
//      if CharInSet(AStr[I], [#32..#127] + AIgnoredChars) then
      if not CharInSet(AStr[I], RemoveChars) then
        lBuilder.Append(AStr[I]);
    Result := lBuilder.ToString;
  finally
    FreeAndNil(lBuilder);
  end;
end;

{ TextHintColorHelper }

procedure TextHintColorHelper.fixWndProc(var aMessage: TMessage);
var
  DC : HDC ;
  R : TRect ;
  OldFont: HFONT;
  OldTextColor: TColorRef;
  Handled : boolean;
begin
  Handled := false;
  if (aMessage.Msg = WM_PAINT) and (Text  = '') and not Focused then
    begin
       Self.WndProc(aMessage);
       Self.Perform(EM_GETRECT, 0, LPARAM(@R));
       DC := GetDC(Handle);
       OldFont := 0;
       OldTextColor := 0;
       try
          Font.Style := [fsItalic];
          OldFont := SelectObject(DC, Font.Handle );
          OldTextColor := SetTextColor(DC, ColorToRGB(GetTextHintColor));
          FillRect(DC, R, 0);
          DrawText(DC, PChar(TextHint), Length(TextHint), R, DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX);
       finally
          SetTextColor(DC, OldTextColor);
          SelectObject(DC, OldFont);
          ReleaseDC(Handle, DC);
       end;
      Handled := true;
    end
  else
    if (Focused) then
      Font.Style := Font.Style - [fsItalic];

  if not Handled then WndProc(aMessage);
end;

function TextHintColorHelper.GetTextHintColor: TColor;
begin
  Result := Tag;
end;

procedure TextHintColorHelper.SetTextHintColor(const Value: TColor);
begin
  Tag :=  Value;
  WindowProc := fixWndProc ;
end;

{ TEnumConverter }

class function TEnumConverter.EnumToInt<T>(const EnumValue: T): Integer;
begin
  Result := 0;
  Move(EnumValue, Result, sizeOf(EnumValue));
end;

class function TEnumConverter.EnumToString<T>(EnumValue: T): string;
begin
  Result := GetEnumName(TypeInfo(T), EnumToInt(EnumValue));
end;

{ TNotifyEventWrapper }

constructor TNotifyEventWrapper.Create(Owner: TComponent; Proc: TProc<TObject>);
begin
  inherited Create(Owner);
  FProc := Proc;
end;

procedure TNotifyEventWrapper.Event(Sender: TObject);
begin
  FProc(Sender);
end;

function AnonProc2NotifyEvent(Owner: TComponent; Proc: TProc<TObject>): TNotifyEvent;
begin
  Result := TNotifyEventWrapper.Create(Owner, Proc).Event;
end;

end.
