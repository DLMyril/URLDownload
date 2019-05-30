unit URLFileUnit;

interface

uses SyncObjs, SysUtils, Classes;

type
  TUrlFileStatus = (ufsCreated, ufsRunning, ufsDownloaded, ufsCounted, ufsFinished, ufsError);
  TUrlFileInfo = class(TObject)
  private
    fUrl: string;
    fDest: string;
    fCount: integer;
    fStatus: TUrlFileStatus;
    fPattern: string;
  public                                                                                                
    Content: TStringList;
    property Url: string read fUrl write fUrl;
    property Dest: string read fDest write fDest;
    property Count: integer read fCount;
    property Pattern: string read fPattern write fPattern;
    property Status: TUrlFileStatus read fStatus write fStatus;
    constructor Create(AURL, ADest, APattern: string);
    function CountPatterns(APattern: string): integer; // sets fCount
  end;
  
  TUrlThread = class(TThread)
  private
    fFileInfo: TUrlFileInfo;
    fLock: TCriticalSection;
    fPattern: string;
  public
    constructor Create(AUrlFile: TUrlFileInfo; ALock: TCriticalSection);
    procedure Execute; override;
  end;

  TURLFileManager = class(TObject)
  private
    class var fURLFileManager: TURLFileManager;
    fFileStatus: array of TUrlFileInfo;
    function GetFileStatus(AUrl: string): TUrlFileInfo;
    procedure SetFileStatus(AUrl: string; const Value: TUrlFileInfo);
    function FindUrl(AUrl: string): TUrlFileInfo;
    function GetCount: integer;
  public
    CSVLock: TCriticalSection;
    CSV: TStringList;
    Threads: array of TUrlThread;
    Handles: array of THandle;
    HasChanged: boolean;
    class function GetInstance: TURLFileManager;
    property FileStatus[AUrl: string]: TUrlFileInfo read GetFileStatus write SetFileStatus;
    property Count: integer read GetCount;
    constructor Create;
    destructor Destroy; override;
    procedure AddToDownload(AUrlFileInfo: TUrlFileInfo);
    procedure StartDownload;
    procedure ReportResults(AFileInfo: TUrlFileInfo);
  end;

const
  UrlFileStatusStr: array[TUrlFileStatus] of string = 
    ('Created', 'Running', 'Downloaded', 'Counted', 'Finished', 'Error');
   

implementation

{ TURLFileManager }

uses URLMon, StrUtils;

procedure TURLFileManager.AddToDownload(AUrlFileInfo: TUrlFileInfo);
begin
  fHasChanged := True; 
  SetLength(fFileStatus, length(fFileStatus) + 1);
  fFileStatus[length(fFileStatus) - 1] := AUrlFileInfo;
end;

constructor TURLFileManager.Create;
begin
  CSVLock := TCriticalSection.Create;
  CSV := TStringList.Create;
  fHasChanged := False;
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

function TURLFileManager.GetCount: integer;
begin
  result := length(fFileStatus);
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

procedure TURLFileManager.ReportResults(AFileInfo: TUrlFileInfo);
begin
  csv.Add(AFileInfo.Url + ',' + AFileInfo.Pattern +',' + IntToStr(AFileInfo.Count));
  fHasChanged := True;
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
var
  i: integer;
begin
  SetLength(Threads, length(fFileStatus));
  SetLength(Handles, length(fFileStatus));
  for i := 0 to (length(fFileStatus)-1) do begin
    Threads[i] := TUrlThread.Create(fFileStatus[i], CSVLock);
    Handles[i] := Threads[i].Handle;
    Threads[i].Execute;
  end;
  fHasChanged := True;
end;                  

{ TUrlFileInfo }

function TUrlFileInfo.CountPatterns(APattern: string): integer;
var
  idx: integer;
  OldCount: integer;
begin
  OldCount := fCount;
  fCount := 0;
  idx := 0;
  repeat
    idx := PosEx(APattern, Content.Text, idx);
    if (idx <> 0) then begin                                                             
      inc(fCount);
      inc(idx);
    end;
  until (idx = 0);
  fStatus := ufsCounted;
  result := fCount;
  if OldCount <> fCount then HasChanged := True;
  
end;

constructor TUrlFileInfo.Create(AUrl, ADest, APattern: string);
begin
  fStatus := ufsCreated;
  fUrl := AUrl;
  fDest := ADest;
  fPattern := APattern;
  fCount := -1;
end;

{ TUrlThread }

constructor TUrlThread.Create(AUrlFile: TUrlFileInfo; ALock: TCriticalSection);
begin
  fFileInfo := AUrlFile;
  fLock := ALock;
end;

procedure TUrlThread.Execute;
begin
  inherited;
  fFileInfo.fStatus := ufsRunning;
  if (URLDownloadToFile(nil, PWideChar(fFileInfo.Url), PWideChar(fFileInfo.Dest), 0, nil) = 0) then begin
    fFileInfo.fStatus := ufsDownloaded;
    fFileInfo.Content.Text := ''; // stub
    fFileInfo.CountPatterns(fPattern);
    fLock.Acquire;
    TURLFileManager.GetInstance.ReportResults(fFileInfo);
    fLock.Release;
    fFileInfo.fStatus := ufsFinished;
  end else begin
    fFileInfo.fStatus := ufsError;
  end;
end;

initialization

finalization

end.
