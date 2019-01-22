{*******************************************************}
{                                                       }
{       Это дополнение для класса THTTPSend из либы     }
{       synapse для работы с сетью и оно реализует      }
{       правильную обработку http редиректов 301 и 302, }
{       ну и пару функций для удобной работы с          }
{       HTTP-хедерами HTTP-запросов                     }
{                                                       }
{       Copyright (C) 2018-2019 Witcher                 }
{                                                       }
{*******************************************************}

unit httpsend_helper;

interface

uses
  SysUtils, Classes, httpsend;

type
  THttpSend_ = class helper for THTTPSend
    function HeaderNameByIndex(index:integer):string;
    function HeaderByName(const HeaderName:string):string;
    function HTTPMethod2(const Method, URL: string): Boolean;
  end;

implementation

{ THttpSend_ }

function THttpSend_.HeaderByName(const HeaderName: string): string;
var
  i : integer;
begin
  for i:=0 to Headers.Count-1 do
    begin
      if LowerCase(HeaderNameByIndex(i)) = lowercase(HeaderName) then
        begin
          Result:=copy(Headers[i],pos(':',
                       LowerCase(Headers[i]))+2,
                       Length(Headers[i])-length(HeaderName));
          break;
        end;
    end;
end;

function THttpSend_.HeaderNameByIndex(index: integer): string;
begin
  if (index > (Headers.Count-1)) or (index < 0) then Exit;
  Result := copy(Headers[index],0, pos(':',Headers[index])-1);
end;

function THttpSend_.HTTPMethod2(const Method, URL: string): Boolean;
var
  Heads: TStringList;
  Cooks: TStringList;
  Redirect: string;
  Doc:TMemoryStream;
begin
  try
    Heads:=TStringList.Create;
    Cooks:=TStringList.Create;
    Doc:=TMemoryStream.Create;
    Doc.LoadFromStream(Document);
    Cooks.Assign(Cookies);
    Heads.Assign(Headers);
    Result := HTTPMethod(Method,URL);
    if (ResultCode=301) or (ResultCode=302) then
      begin
        Redirect:=HeaderByName('location');
        Headers.Assign(Heads);
        Document.Clear;
        Document.LoadFromStream(Doc);
        Cookies.Assign(Cooks);
        Result := HTTPMethod(Method,Redirect);
       end;
  finally
    FreeAndNil(Heads);
    FreeAndNil(Cooks);
    FreeAndNil(Doc)
  end;
end;

end.
