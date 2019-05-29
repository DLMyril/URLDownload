unit URLFileUnit;

interface

uses SyncObjs, SysUtils, Classes;

type
  TUrlFileStatus = (ufsCreated, ufsRunning, ufsDownloaded, ufsCounted, ufsFinished);
  TUrlFileInfo = class(TObject)
  private
    fUrl: string;
    fDest: string;
    fCount: integer;
    fStatus: TUrlFileStatus;
  public
    Content: TStringList;
    property Url: string read fUrl write fUrl;
    property Dest: string read fDest write fDest;
    property Count: integer read fCount;
    property Status: TUrlFileStatus read fStatus write fStatus;
    constructor Create(AURL, ADest: string);
    destructor Destroy; override;
    function CountPatterns(APattern: string): integer; // sets fCount
  end;
  
  TUrlThread = class(TThread)
  private
    fFileInfo: TUrlFileInfo;
    fLock: TCriticalSection;
  public
    constructor Create(AUrlFile: TUrlFileInfo; ALock: TCriticalSection);
  end;

  TURLFileManager = class(TObject)
  private
    class var fURLFileManager: TURLFileManager;
    fFileStatus: array of TUrlFileInfo;
    function GetFileStatus(AUrl: string): TUrlFileInfo;
    procedure SetFileStatus(AUrl: string; const Value: TUrlFileInfo);
    function FindUrl(AUrl: string): TUrlFileInfo;
  public
    CSVLock: TCriticalSection;
    CSV: TStringList;
    Threads: array of TUrlThread;
    Handles: array of THandle;
    class function GetInstance: TURLFileManager;
    property FileStatus[AUrl: string]: TUrlFileInfo read GetFileStatus write SetFileStatus;
    constructor Create;
    destructor Destroy; override;
    procedure AddToDownload(AUrlFileInfo: TUrlFileInfo);
    procedure StartDownload;
  end;

implementation

{ TURLFileManager }

procedure TURLFileManager.AddToDownload(AUrlFileInfo: TUrlFileInfo);
begin
  SetLength(fFileStatus, length(fFileStatus) + 1);
  fFileStatus[length(fFileStatus) - 1] := AUrlFileInfo;
end;

constructor TURLFileManager.Create;
begin
  CSVLock := TCriticalSection.Create;
  CSV := TStringList.Create;
end;

destructor TURLFileManager.Destroy;
begin
  if assigned(CSVLock) then CSVLock.Free;
  if assigned(CSV) then begin
    while (CSV.Count > 0) do CSV.Delete(0);
    CSV.Free;
  end;
  
  if assigned(fURLFileManager) then fURLFileManager := nil;
  
  inherited;
end;

function TURLFileManager.FindUrl(AUrl: string): TUrlFileInfo;
var
  idx: integer;
begin
  result := nil;
  idx := 0;
  while not assigned(result) and (idx < length(fFileStatus)) and (fFileStatus[idx].Url <> AUrl) do inc(idx);
  if idx <> -1 then result := fFileStatus[idx];
end;

function TURLFileManager.GetFileStatus(AUrl: string): TUrlFileInfo;
begin
  result := FindUrl(AUrl);
end;

class function TURLFileManager.GetInstance: TURLFileManager;
begin
  if not assigned(fURLFileManager) then fURLFileManager := TURLFileManager.Create;
  result := fURLFileManager;
end;

procedure TURLFileManager.SetFileStatus(AUrl: string; const Value: TUrlFileInfo);
var
  FoundFile: TUrlFileInfo;
begin
  FoundFile := FindUrl(AUrl);
  if assigned(FoundFile) then begin
    FoundFile.fUrl := Value.fUrl;
    FoundFile.fDest := Value.fDest;
    FoundFile.fCount := Value.fCount;
    FoundFile.Content.Text := Value.Content.Text;
  end else begin
    AddToDownload(Value);
  end;
end;

procedure TURLFileManager.StartDownload;
begin
   // stub
end;

{ TUrlFileInfo }

function TUrlFileInfo.CountPatterns(APattern: string): integer;
begin
  result := -1; // stub
end;

constructor TUrlFileInfo.Create(AUrl, ADest: string);
begin
  fStatus := ufsCreated;
  fUrl := AUrl;
  fDest := ADest;
  fCount := -1;
end;

destructor TUrlFileInfo.Destroy;
begin
  // stub
  inherited;
end;

{ TUrlThread }

constructor TUrlThread.Create(AUrlFile: TUrlFileInfo; ALock: TCriticalSection);
begin
  fFileInfo := AUrlFile;
  fLock := ALock;
end;

initialization

finalization

end.
