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
    procedure CopyTo(Destination: TUrlFileInfo);
  end;
  
  TUrlThread = class(TThread)
  private
    fFileInfo: TUrlFileInfo;
    fLock: TCriticalSection;
  public
    constructor Create(AUrlFileInfo: TUrlFileInfo; ALock: TCriticalSection);
    destructor Destroy; override;
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
    procedure ReportResults(AFileInfo: TUrlFileInfo); // call withing a critical section using CSVLock
  end;

const
  UrlFileStatusStr: array[TUrlFileStatus] of string = 
    ('Created', 'Running', 'Downloaded', 'Counted', 'Finished', 'Error');
   

implementation

{ TURLFileManager }

uses URLMon, StrUtils;

procedure TURLFileManager.AddToDownload(AUrlFileInfo: TUrlFileInfo);
var
  WorkUrlInfo: TUrlFileInfo;
begin
  HasChanged := True;
  WorkUrlInfo := FindUrl(AUrlFileInfo.Url);
  if not assigned(WorkUrlInfo) then begin // adds a new entry
    SetLength(fFileStatus, length(fFileStatus) + 1);
    fFileStatus[length(fFileStatus) - 1] := AUrlFileInfo;
  end else begin
    AUrlFileInfo.CopyTo(WorkUrlInfo); // update an existing entry
  end;
end;

constructor TURLFileManager.Create;
begin
  CSVLock := TCriticalSection.Create;
  CSV := TStringList.Create;
  HasChanged := False;
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
  i, l: integer;
begin
  i := 0;
  l := length(fFileStatus);
  while (i < l) and (fFileStatus[i].Url <> AUrl) do inc(i);
  if i = l then result := nil else result := fFileStatus[i];
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

procedure TURLFileManager.ReportResults(AFileInfo: TUrlFileInfo);  // only call inside CriticalSection
var
  UpdatedFileInfo: TUrlFileInfo;
begin
  UpdatedFileInfo := FindUrl(AFileInfo.Url);
  AFileInfo.CopyTo(UpdatedFileInfo);
  csv.Add(AFileInfo.Url + ',' + AFileInfo.Pattern +',' + IntToStr(AFileInfo.Count));
  HasChanged := True;
end;

procedure TURLFileManager.SetFileStatus(AUrl: string; const Value: TUrlFileInfo);
var
  FoundFile: TUrlFileInfo;
begin
  FoundFile := FindUrl(AUrl);
  if assigned(Value) and assigned(FoundFile) then begin
    Value.CopyTo(FoundFile);
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
  HasChanged := True;
end;                  

{ TUrlFileInfo }

procedure TUrlFileInfo.CopyTo(Destination: TUrlFileInfo);
begin
  if assigned(Destination) then begin
    Destination.fUrl := fUrl;
    Destination.fDest := fDest;
    Destination.fCount := fCount;
    Destination.fStatus := fStatus;
    Destination.fPattern := fPattern;
    Destination.Content.Text := Content.Text;
  end;
end;

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
  if (OldCount <> fCount) then TUrlFileManager.GetInstance.HasChanged := True;
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

constructor TUrlThread.Create(AUrlFileInfo: TUrlFileInfo; ALock: TCriticalSection);
begin
  fFileInfo := TURLFileInfo.Create('','','');
  AUrlFileInfo.CopyTo(fFileInfo);
  fLock := ALock;
  inherited Create(False);
end;

destructor TUrlThread.Destroy;
begin
  if assigned(fFileInfo) then fFileInfo.Free;
  
  inherited;
end;

procedure TUrlThread.Execute;
begin
  inherited;
  fFileInfo.fStatus := ufsRunning;
  if (URLDownloadToFile(nil, PWideChar(fFileInfo.Url), PWideChar(fFileInfo.Dest), 0, nil) = 0) then begin
    fFileInfo.fStatus := ufsDownloaded;
    fFileInfo.Content.LoadFromFile(fFileInfo.Dest);
    fFileInfo.CountPatterns(fFileInfo.Pattern);
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
