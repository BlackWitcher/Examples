{*******************************************************}
{                                                       }
{  ћодуль дл€ скачивани€ по FTP/HTTP                    }
{                                                       }
{  Copyright (C) 2018-2019 Witcher                      }
{                                                       }
{*******************************************************}

unit Downloaders;

interface

uses
  Windows, SysUtils, Classes, ftpsend, blcksock, StrUtils, httpsend, Dialogs,
  ssl_openssl;

type
  TProgressNotifyEvent = procedure(Sender: TObject; Total, Current : Int64; Percent : Byte) of object;

  //FTP-менеджер
  TFTPMan = class
  private
    FTPSend : TFTPSend;
    FPort: integer;
    FPass: string;
    FHost: string;
    FUser: string;
    FLog : TStrings;
    FPassive: Boolean;
    FConnected: Boolean;
    FOnProgress: TProgressNotifyEvent;
    FTotalBytes, FCurrentBytes : Int64;
    FBinary: Boolean;
    FCmdList : TStrings;
    procedure SetHost(const Value: string);
    procedure SetPass(const Value: string);
    procedure SetPort(const Value: integer);
    procedure SetUser(const Value: string);
    procedure Status(Sender: TObject; Response: Boolean; const Value: string);
    procedure DSockStatus(Sender: TObject; Reason: THookSocketReason; const Value: String);
    procedure SetPassiveMode(const Value: Boolean);
    procedure SetConnected(const Value: Boolean);
    function GetFTPList: TFTPList;
    procedure SetBinaryMode(const Value: Boolean);
    function GetResultCode: integer;
    function GetResultString: string;
    function GetResumeStatus: Boolean;
    function GetServerCommands: TStrings;
  protected
    procedure DoOnProgress(Total, Value : Int64; Percent : Byte); dynamic;
  public
    property OnProgress : TProgressNotifyEvent read FOnProgress write FOnProgress;

    property Connected : Boolean read FConnected write SetConnected;
    property Host : string read FHost write SetHost;
    property User : string read FUser write SetUser;
    property Password : string read FPass write SetPass;
    property Port : integer read FPort write SetPort;
    property Passive : Boolean read FPassive write SetPassiveMode;
    property Binary : Boolean read FBinary write SetBinaryMode;
    property Log : TStrings read FLog;
    property FTPList : TFTPList read GetFTPList;
    property ResultCode : integer read GetResultCode;
    property ResultString : string read GetResultString;
    property CanResume : Boolean read GetResumeStatus;
    property ServerCommands : TStrings read FCmdList;

    function Login : Boolean; overload;
    function Login(const AUser, APassword, AHost : string; APort : integer; APassive : Boolean = True) : Boolean; overload;
    function Logout : Boolean; overload;

    procedure ResetData;
    function CurrentDir : string;
    function ChangeToParentDir : Boolean;
    function ChangeToRoot : Boolean;
    function ChangeDir(const Dir : string) : Boolean;
    function List(const Dir : string; const NameList : Boolean) : Boolean;
    function IsFileExists(const FTPFilename : string) : Boolean;
    function Download(const FTPFilename, Filename : string) : Boolean;
    function Upload(const Filename, FTPFilename : string) : Boolean;
    function RenameFile(const OldName, NewName: string): Boolean;
    function DeleteFile(const FileName: string): Boolean;
    function DeleteDirectory(const DirName : string) : Boolean;
    function CreateDir(const Directory: string): Boolean;
    function GetFileSize(const FTPFilename : string) : Int64;
    function ExecCommand(const FtpCommand : string) : string;

    constructor Create;
    destructor Destroy; override;
  end;

  //HTTP-менеджер
  THTTPMan = class
  private
    FOnProgress: TProgressNotifyEvent;
    FFileSize, FDownloaded : int64;
    procedure DownloadProgress(Sender: TObject; Reason: THookSocketReason; const Value: string);//–еальный обработчик OnStatus
  protected
    procedure DoOnProgress(Total, Value : Int64; Percent : Byte); dynamic;
  public
    constructor Create;
    destructor Destroy; override;

    function GetSize(URL : string) : Int64; overload;
    function GetSize(const URL : string; out ResultCode : integer; out ResultString : string) : Int64; overload;

    function GetFile(URL : string) : TMemoryStream; overload;
    function GetFile(const URL : string; out ResultCode : integer; out ResultString : string) : TMemoryStream; overload;

    function Download(const URL, Filename : string) : Boolean;

    property OnProgress : TProgressNotifyEvent read FOnProgress write FOnProgress;
    property Downloaded : int64 read FDownloaded;
  end;


  function FTPd : TFTPMan;
  function HTTPd : THTTPMan;

implementation

uses
  httpsend_helper, mTools;

var
  FtpInstance : TFTPMan;
  HttpInstance : THTTPMan;

function FTPd : TFTPMan;
var
   newFtpInstance: TFTPMan;
begin
   if (FtpInstance = nil) then
     begin
        newFtpInstance := TFTPMan.Create;
        if InterlockedCompareExchangePointer(Pointer(FtpInstance), Pointer(newFtpInstance), nil) <> nil then
          FreeAndNil(newFtpInstance);
     end;
   Result := FtpInstance;
end;

function HTTPd : THTTPMan;
var
   newHttpInstance: THTTPMan;
begin
   if (HttpInstance = nil) then
     begin
        newHttpInstance := THTTPMan.Create;
        if InterlockedCompareExchangePointer(Pointer(HttpInstance), Pointer(newHttpInstance), nil) <> nil then
          FreeAndNil(newHttpInstance);
     end;
   Result := HttpInstance;
end;

function GetLocalFileSize(const aFilename: String): Int64;
var
  info: TWin32FileAttributeData;
begin
  result := -1;

  if NOT GetFileAttributesEx(PWideChar(aFileName), GetFileExInfoStandard, @info) then
    EXIT;

  result := Int64(info.nFileSizeLow) or Int64(info.nFileSizeHigh shl 32);
end;

{ TFTP }

function TFTPMan.ChangeDir(const Dir: string): Boolean;
begin
  Result := FTPSend.ChangeWorkingDir(Dir);
  if Result then FTPSend.List(EmptyStr, False);
end;

function TFTPMan.ChangeToParentDir: Boolean;
begin
  Result := FTPSend.ChangeToParentDir;
  if Result then FTPSend.List(EmptyStr, False);
end;

function TFTPMan.ChangeToRoot: Boolean;
begin
  Result := FTPSend.ChangeToRootDir;
  if Result then FTPSend.List(EmptyStr, False);
end;

constructor TFTPMan.Create;
begin
  inherited Create;
  FTPSend := TFTPSend.Create;
  FLog := TStringList.Create;
  FTPSend.OnStatus := Status;
  FTotalBytes := 0;
  FCurrentBytes := 0;
  Self.Binary := True;
  FCmdList := TStringList.Create;
end;

function TFTPMan.CreateDir(const Directory: string): Boolean;
begin
  Result := FTPSend.CreateDir(Directory);
end;

function TFTPMan.CurrentDir: string;
begin
  Result := FTPSend.GetCurrentDir;
end;

function TFTPMan.DeleteDirectory(const DirName: string): Boolean;
begin
  Result := FTPSend.DeleteDir(DirName);
end;

function TFTPMan.DeleteFile(const FileName: string): Boolean;
begin
  Result := FTPSend.DeleteFile(FileName);
end;

destructor TFTPMan.Destroy;
begin
  if Assigned(FTPSend) then FreeAndNil(FTPSend);
  if Assigned(FLog) then FreeAndNil(FLog);
  if Assigned(FCmdList) then FreeAndNil(FCmdList);

  inherited;
end;

procedure TFTPMan.DoOnProgress(Total, Value: Int64; Percent: Byte);
begin
  if Assigned(FOnProgress) then FOnProgress(Self, Total, Value, Percent);
end;

function TFTPMan.Download(const FTPFilename, Filename: string): Boolean;
begin
  FTPSend.DSock.OnStatus := DSockStatus;
  FTotalBytes := FTPSend.FileSize(FTPFilename);
  FCurrentBytes := 0;
  if FTPSend.RetrieveFile(FTPFilename, FTPSend.CanResume) then
    begin
      //скачивание прошло успешно - сохран€ем файл
      if FileExists(Filename) then
        begin
          try
            DeleteFile(Filename);
          except
            Result := False;
            Exit;
          end;
        end;
      FTPSend.DataStream.SaveToFile(Filename);
      Result := FileExists(Filename);
    end
  else
    begin
      //загрузка не удалась - выводим сообщение в лог
      FLog.Add('Ч> Ќе удалось скачать файл "' + FTPFilename + '"');
      Result := False;
    end;
  FTPSend.DSock.OnStatus := nil;
  FCurrentBytes := FTotalBytes;
  FTotalBytes := 0;
end;

procedure TFTPMan.DSockStatus(Sender: TObject; Reason: THookSocketReason;
  const Value: String);
var
  Percent : Byte;
  Total : Int64;
  Current : integer;
begin
  if Reason in [HR_ReadCount, HR_WriteCount] then
    begin
      Total := FTotalBytes;
      Inc(FCurrentBytes, StrToIntDef(Value, 0));
      Current := FCurrentBytes;
      Percent := round(100 * (Current / Total));
      DoOnProgress(Total, Current, Percent);
    end;
end;

function TFTPMan.ExecCommand(const FtpCommand: string): string;
begin
  Result := EmptyStr;
  if FCmdList.Count <= 0 then Exit;
  FTPSend.FTPCommand(FtpCommand);
  Result := FTPSend.FullResult.Text;
end;

function TFTPMan.GetFileSize(const FTPFilename: string): Int64;
begin
  Result := FTPSend.FileSize(FTPFilename);
end;

function TFTPMan.GetFTPList: TFTPList;
begin
  Result := FTPSend.FtpList;
end;

function TFTPMan.GetResultCode: integer;
begin
  Result := FTPSend.ResultCode;
end;

function TFTPMan.GetResultString: string;
begin
  Result := FTPSend.ResultString;
end;

function TFTPMan.GetResumeStatus: Boolean;
begin
  Result := FTPSend.CanResume;
end;

function TFTPMan.GetServerCommands: TStrings;
begin
  Result := TStringList.Create;
  if FTPSend.FTPCommand('HELP')=214 then
    begin
      FTPSend.FullResult.Delete(0);
      FTPSend.FullResult.Delete(FTPSend.FullResult.Count-1);
      FCmdList.Clear;
      FCmdList.DelimitedText := FTPSend.FullResult.Text;
    end;
  Result.Assign(FCmdList);
end;

function TFTPMan.IsFileExists(const FTPFilename: string): Boolean;
begin
  Result := FTPSend.FileSize(FTPFilename) >= 0;
end;

function TFTPMan.List(const Dir: string; const NameList: Boolean): Boolean;
begin
  Result := FTPSend.List(Dir, NameList);
end;

function TFTPMan.Login: Boolean;
begin
  FTPSend.UserName := FUser;
  FTPSend.Password := FPass;
  FTPSend.TargetHost := FHost;
  FTPSend.TargetPort := IntToStr(FPort);
  FTPSend.PassiveMode := FPassive;
  Result := FTPSend.Login;
  if Result then
    begin
      GetServerCommands;
      FLog.Add('Ч> —оединение установлено.');
    end
  else
    FLog.Add('Ч> Ќе удалось установить соединение.');
end;

function TFTPMan.Login(const AUser, APassword, AHost: string; APort: integer;
  APassive: Boolean): Boolean;
begin
  User := AUser;
  Password := APassword;
  Host := AHost;
  Port := APort;
  Passive := APassive;
  Result := Self.Login;
  FConnected := Result;
end;

function TFTPMan.Logout: Boolean;
begin
  Result := FTPSend.Logout;
  FConnected := not Result;
  if Result then
    FLog.Add('Ч> ќтключение произведено.')
  else
    FLog.Add('Ч> Ќе удалось прервать соединение.');
end;

function TFTPMan.RenameFile(const OldName, NewName: string): Boolean;
begin
  Result := FTPSend.RenameFile(OldName, NewName);
end;

procedure TFTPMan.ResetData;
begin
  FTPSend.DataStream.Clear;
end;

procedure TFTPMan.SetBinaryMode(const Value: Boolean);
begin
  if Value <> FBinary then
  begin
    FBinary := Value;
    FTPSend.BinaryMode := FBinary;
  end;
end;

procedure TFTPMan.SetConnected(const Value: Boolean);
begin
  if Value <> FConnected then
    begin
      if Value then FConnected := Self.Login else FConnected := Self.Logout;
    end;
end;

procedure TFTPMan.SetHost(const Value: string);
begin
  if FHost <> Value then
    FHost := Value;
end;

procedure TFTPMan.SetPass(const Value: string);
begin
  if FPass <> Value then
    FPass := Value;
end;

procedure TFTPMan.SetPassiveMode(const Value: Boolean);
begin
  if Value <> FPassive then
    begin
      FPassive := Value;
      FTPSend.PassiveMode := FPassive;
    end;
end;

procedure TFTPMan.SetPort(const Value: integer);
begin
  if FPort <> Value then
    FPort := Value;
end;

procedure TFTPMan.SetUser(const Value: string);
begin
  if FUser <> Value then
    FUser := Value;
end;

procedure TFTPMan.Status(Sender: TObject; Response: Boolean; const Value: string);
begin
  if Response then
    FLog.Add('Ч> '+Value)
  else
    begin
      if pos('REST', Value) > 0 then
        FCurrentBytes := StrToInt64Def(copy(Value,Pos('REST',Value)+5,Length(Value)-Pos('REST',Value)-4),0);
      FLog.Add('<Ч '+Value);
    end;
end;

function TFTPMan.Upload(const Filename, FTPFilename: string): Boolean;
begin
  Result := False;
  if not FileExists(Filename) then Exit;

  FTPSend.DataStream.LoadFromFile(FileName);

  FTotalBytes := GetLocalFileSize(Filename);
  FCurrentBytes := 0;

  FTPSend.DSock.OnStatus:=DSockStatus;
  Result := FTPSend.StoreFile(ExtractFileName(FTPFilename), FTPd.CanResume);
  FTPSend.DSock.OnStatus:=nil;
  if Result then
    begin
      //загрузка прошла успешно - обновл€ем список объектов
      FTPSend.List(EmptyStr,False);
      FLog.Add('Ч> ”спешно закачан файл "' + FTPFilename + '"');
    end
  else
    begin
      //загрузка не удалась - выводим сообщение в лог
      FLog.Add('Ч> Ќе удалось закачать файл "' + Filename + '"');
      Result := False;
    end;
  //обнул€ем прогресс
  FTPSend.DSock.OnStatus := nil;
  FCurrentBytes := FTotalBytes;
  FTotalBytes := 0;
end;

{ THTTPMan }

constructor THTTPMan.Create;
begin
  FFileSize := -1;
  FDownloaded := 0;
end;

destructor THTTPMan.Destroy;
begin
  inherited;
end;

procedure THTTPMan.DoOnProgress(Total, Value: Int64; Percent: Byte);
begin
  if Assigned(FOnProgress) then FOnProgress(Self, Total, Value, Percent);
end;

function THTTPMan.Download(const URL, Filename : string): Boolean;
var
  MS : TMemoryStream;
begin
  //—качивание ссылки в файл
  MS := TMemoryStream.Create;
  MS.Position := 0;
  try
    MS := GetFile(URL);
    try
      if FileExists(Filename) then
        Result := DeleteFile(Filename);
    except
      Result := False;
      Exit;
    end;
    MS.SaveToFile(Filename);
    Result := FileExists(Filename);
  finally
    FreeAndNil(MS);
  end;
end;

procedure THTTPMan.DownloadProgress(Sender: TObject; Reason: THookSocketReason;
  const Value: string);
var
  Percent : Byte;
  Total : Int64;
  Current : integer;
begin
  //ќбработчик прогресса
  if Reason = HR_ReadCount then
    begin
      Total := FFileSize;
      Inc(FDownloaded, StrToIntDef(Value, 0));
      Current := FDownloaded;
      Percent := Trunc((Current/Total)*100);
      DoOnProgress(Total, Current, Percent);
    end;
end;

function THTTPMan.GetFile(const URL: string; out ResultCode: integer;
  out ResultString: string): TMemoryStream;
var
  HTTP : THTTPSend;
  ResponseCode : Integer;
  ResultStr : string;
begin
  FFileSize := GetSize(URL, ResponseCode, ResultStr);
  ResultCode := ResponseCode;
  ResultString := ResultStr;
  Result := nil;
  FDownloaded := 0;

  if LowerCase(ResultStr) <> 'ok' then Exit; //ќшибка при подсчЄте размера файла!

  Result := TMemoryStream.Create;
  HTTP := THTTPSend.Create;
  HTTP.Sock.OnStatus := DownloadProgress;
  try
    if HTTP.HTTPMethod('GET', URL) then
      begin
        Result.LoadFromStream(HTTP.Document);
      end;
    ResultCode := HTTP.ResultCode;
    ResultString := HTTP.ResultString;
  finally
    FreeAndNil(HTTP);
  end;
end;

function THTTPMan.GetFile(URL: string): TMemoryStream;
var
  HTTP : THTTPSend;
begin
  //ѕолучим размер файла
  FFileSize := GetSize(URL);
  FDownloaded := 0;
  HTTP := THTTPSend.Create;
  HTTP.Sock.OnStatus := DownloadProgress;
  Result := TMemoryStream.Create;
  try
    if HTTP.HTTPMethod('GET', URL) then
      begin
        Result.LoadFromStream(HTTP.Document);
        Result.Position := 0;
      end;
    // OK, 200 ShowMessage(Http.ResultString + #13#10 + IntToStr(Http.ResultCode));
  finally
    FreeAndNil(HTTP);
  end;
end;

function THTTPMan.GetSize(const URL: string; out ResultCode: integer;
  out ResultString: string): Int64;
var
  i, p, p1 : integer;
  size, s : string;
  ch: char;
  HTTP : THTTPSend;
begin
  Result := -1;
  HTTP := THTTPSend.Create;
  try
    if HTTP.HTTPMethod('HEAD',URL) then
      begin
        for I := 0 to HTTP.Headers.Count - 1 do
          begin
            if pos('content-length',lowercase(HTTP.Headers[i])) > 0 then
              begin
                size:='';
                for ch in HTTP.Headers[i] do
                  if CharInSet(ch, ['0'..'9']) then
                     size := size + ch;
                if not TryStrToInt64(size, Result) then
                  Result := 0;//Result + Length(FHTTPSend.Headers.Text);
                break;
              end;
          end;

        if Result = -1 then
          begin
            for i := 0 to HTTP.Headers.Count - 1 do
              begin
                if pos('content-disposition', LowerCase(HTTP.Headers[i])) > 0 then
                  begin
                    p := pos('size=', LowerCase(HTTP.Headers[i]));
                    if p > 0 then
                      begin
                        p1 := PosEx(';', HTTP.Headers[i], p);
                        if (p1 > 0) and (p1 > p) then
                          begin
                            s := copy(HTTP.Headers[i], p, p1 - p);
                            size := '';
                            for ch in s do
                              if CharInSet(ch, ['0'..'9']) then
                                size := size + ch;
                            if not TryStrToInt64(size, Result) then Result := -1;
                          end
                        else
                          Result := -1;
                      end;
                  end;
              end;
          end;
      end;
    ResultCode := HTTP.ResultCode;
    ResultString := HTTP.ResultString;
  finally
    FreeAndNil(HTTP);
  end;
end;

function THTTPMan.GetSize(URL: string): Int64;
var
  i, p, p1 : integer;
  size, s : string;
  ch: char;
  HTTP : THTTPSend;
begin
  Result := -1;
  HTTP := THTTPSend.Create;
  try
    if HTTP.HTTPMethod('HEAD',URL) then
      begin
//        ShowMessage(HTTP.Headers.Text);
        for I := 0 to HTTP.Headers.Count - 1 do
          begin
            if pos('content-length', LowerCase(HTTP.Headers[i])) > 0 then
              begin
                size:='';
                for ch in HTTP.Headers[i] do
                  if CharInSet(ch, ['0'..'9']) then
                     size := size + ch;
                if not TryStrToInt64(size, Result) then
                  Result := -1;//Result + Length(FHTTPSend.Headers.Text);
                break;
              end;
          end;

        if Result = -1 then
          begin
            for i := 0 to HTTP.Headers.Count - 1 do
              begin
                if pos('content-disposition', LowerCase(HTTP.Headers[i])) > 0 then
                  begin
                    p := pos('size=', LowerCase(HTTP.Headers[i]));
                    if p > 0 then
                      begin
                        p1 := PosEx(';', HTTP.Headers[i], p);
                        if (p1 > 0) and (p1 > p) then
                          begin
                            s := copy(HTTP.Headers[i], p, p1 - p);
                            size := '';
                            for ch in s do
                              if CharInSet(ch, ['0'..'9']) then
                                size := size + ch;
                            if not TryStrToInt64(size, Result) then Result := -1;
                          end
                        else
                          Result := -1;
                      end;
                  end;
              end;
          end;
      end;
  finally
    FreeAndNil(HTTP);
  end;
end;

end.
