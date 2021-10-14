unit LoggerPro;
{ <@abstract(Contains the LoggerPro core. Include this if you want to create your own logger, otherwise you can use the global one using @link(LoggerPro.GlobalLogger.pas))
  @author(Daniele Teti) }

{$SCOPEDENUMS ON}

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  System.Classes,
  ProcessingThread,
  ThreadSafeQueueU;

var
  DefaultLoggerProMainQueueSize: Cardinal = 50000;
  DefaultLoggerProAppenderQueueSize: Cardinal = 50000;

type
  TLogType = (Debug = 0, Info, Warning, Error);
  TLogErrorReason = (QueueFull);
  TLogErrorAction = (SkipNewest, DiscardOlder);
  TLogExtendedInfo = (EIUserName, EIComputerName, EIProcessName, EIProcessID, EIDeviceID { mobile });
  TLoggerProExtendedInfo = set of TLogExtendedInfo;

  { @abstract(Represent the single log item)
    Each call to some kind of log method is wrapped in a @link(TLogItem)
    instance and passed down the layour of LoggerPro. }
  TLogItem = class sealed
  private
    FType: TLogType;
    FMessage: string;
    FTag: string;
    FTimeStamp: TDateTime;
    FThreadID: TThreadID;
    function GetLogTypeAsString: string;
  public
    constructor Create(const aType: TLogType; const aMessage: string; const aTag: string); overload;
    constructor Create(const aType: TLogType; const aMessage: string; const aTag: string; const aTimeStamp: TDateTime;
      const aThreadID: TThreadID); overload;

    function Clone: TLogItem;
    { @abstract(The type of the log)
      Log can be one of the following types:
      @unorderedlist(
      @item(DEBUG)
      @item(INFO)
      @item(WARNING)
      @item(ERROR)
      ) }
    property LogType: TLogType read FType;
    { @abstract(The text of the log message) }
    property LogMessage: string read FMessage;
    { @abstract(The tag of the log message) }
    property LogTag: string read FTag;
    { @abstract(The timestamp when the @link(TLogItem) is generated) }
    property TimeStamp: TDateTime read FTimeStamp;
    { @abstract(The IDof the thread which generated the log item) }
    property ThreadID: TThreadID read FThreadID;
    { @abstract(The type of the log converted in string) }
    property LogTypeAsString: string read GetLogTypeAsString;
  end;

  TLoggerProAppenderErrorEvent = reference to procedure(const AppenderClassName: string; const aFailedLogItem: TLogItem;
    const Reason: TLogErrorReason; var Action: TLogErrorAction);

  TLoggerProEventsHandler = class sealed
  public
    OnAppenderError: TLoggerProAppenderErrorEvent;
  end;

  { @abstract(Interface implemented by all the classes used as appenders) }
  ILogAppender = interface
    ['{58AFB557-C594-4A4B-8DC9-0F13B37F60CB}']
    { @abstract(This method is internally called by LoggerPro to initialize the appender) }
    procedure Setup;
    { @abstract(This method is called at each log item represented by @link(TLogItem))
      The appender should be as-fast-as-it-can to handle the message, however
      each appender runs in a separated thread. }
    procedure WriteLog(const aLogItem: TLogItem);
    { @abstract(This method is internally called by LoggerPro to deinitialize the appender) }
    procedure TearDown;
    // { @abstract(Enable or disable the log appender. Is used internally by LoggerPro but must be
    // implemented by each logappender. A simple @code(if enabled then dolog) is enough }
    // procedure SetEnabled(const Value: Boolean);
    // { @abstract(Returns if the logappender is currently enabled or not. }
    // function IsEnabled: Boolean;
    { @abstract(Set a custom log level for this appender. This value must be lower than the global LogWriter log level. }
    procedure SetLogLevel(const Value: TLogType);
    { @abstract(Get the loglevel for the appender. }
    function GetLogLevel: TLogType;
    { @abstract(If the appender is disabled, this method is called at each new
      logitem. This method should not raise exceptions and should try to restart the appender
      at specified time and only if some appropriate seconds/miutes are elapsed between the
      LastErrorTimestamp. }
    procedure TryToRestart(var Restarted: Boolean);

    procedure SetLastErrorTimeStamp(const LastErrorTimeStamp: TDateTime);
    function GetLastErrorTimeStamp: TDateTime;
    property LastErrorTimeStamp: TDateTime read GetLastErrorTimeStamp write SetLastErrorTimeStamp;
  end;

  ELoggerPro = class(Exception)

  end;

  ILogWriter = interface
    ['{A717A040-4493-458F-91B2-6F6E2AFB496F}']
    procedure Debug(const aMessage: string; const aTag: string); overload;
    procedure Debug(const aMessage: string; const aParams: array of TVarRec; const aTag: string); overload;
    procedure DebugFmt(const aMessage: string; const aParams: array of TVarRec; const aTag: string); deprecated;

    procedure Info(const aMessage: string; const aTag: string); overload;
    procedure Info(const aMessage: string; const aParams: array of TVarRec; const aTag: string); overload;
    procedure InfoFmt(const aMessage: string; const aParams: array of TVarRec; const aTag: string); deprecated;

    procedure Warn(const aMessage: string; const aTag: string); overload;
    procedure Warn(const aMessage: string; const aParams: array of TVarRec; const aTag: string); overload;
    procedure WarnFmt(const aMessage: string; const aParams: array of TVarRec; const aTag: string); deprecated;

    procedure Error(const aMessage: string; const aTag: string); overload;
    procedure Error(const aMessage: string; const aParams: array of TVarRec; const aTag: string); overload;
    procedure ErrorFmt(const aMessage: string; const aParams: array of TVarRec; const aTag: string); deprecated;

    procedure Log(const aType: TLogType; const aMessage: string; const aTag: string); overload;
    procedure Log(const aType: TLogType; const aMessage: string; const aParams: array of const; const aTag: string); overload;
    procedure LogFmt(const aType: TLogType; const aMessage: string; const aParams: array of const; const aTag: string); deprecated;

    function GetAppendersClassNames: TArray<string>;
    function GetAppenders(const Index: Integer): ILogAppender;
    property Appenders[const index: Integer]: ILogAppender read GetAppenders;
    procedure AddAppender(const aAppenders: ILogAppender);
    procedure DelAppender(const aAppenders: ILogAppender);
    function AppendersCount(): Integer;
  end;

  TLogAppenderList = TList<ILogAppender>;

  TAppenderThread = class(TProcessingQueueThread<TLogItem>)
  protected
    type
      TAppenderStatus = (BeforeSetup, Running, WaitAfterFail, ToRestart, BeforeTearDown);
  private
    FLogAppender: ILogAppender;
    FStatus: TAppenderStatus;
    FSetupFailCount: Integer;
    function GetLogLevel: TLogType;
  protected
    procedure DoAfterExecute; override;
    procedure DoBeforeExecute; override;
    procedure DoOnNewItem(const AItem: TLogItem); override;
    procedure Process(var ADoPause: Boolean); override;
  public
    constructor Create(ALogAppender: ILogAppender);
    property LogLevel: TLogType read GetLogLevel;
  end;

  TLoggerThread = class(TProcessingQueueThread<TLogItem>)
  private
    FEventsHandlers: TLoggerProEventsHandler;
    FAppenderList: TObjectList<TAppenderThread>;
    procedure DoOnAppenderError(const FailAppenderClassName: string; const aFailedLogItem: TLogItem; const aReason: TLogErrorReason;
      var aAction: TLogErrorAction);
    procedure SetEventsHandlers(const Value: TLoggerProEventsHandler);
  protected
    procedure DoOnNewItem(const AItem: TLogItem); override;
    procedure DoTerminate; override;
  public
    constructor Create(aAppenders: TLogAppenderList);
    property EventsHandlers: TLoggerProEventsHandler read FEventsHandlers write SetEventsHandlers;
  end;

  TLoggerProInterfacedObject = class(TInterfacedObject)
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

  TLogWriter = class(TLoggerProInterfacedObject, ILogWriter)
  private
    FLoggerThread: TLoggerThread;
    FLogAppenders: TLogAppenderList;
    FFreeAllowed: Boolean;
    FLogLevel: TLogType;
    procedure Initialize(aEventsHandler: TLoggerProEventsHandler);
    function GetAppendersClassNames: TArray<string>;
  public
    function GetAppenders(const Index: Integer): ILogAppender;
    procedure AddAppender(const aAppender: ILogAppender);
    procedure DelAppender(const aAppender: ILogAppender);
    function AppendersCount(): Integer;
    constructor Create(aLogLevel: TLogType = TLogType.Debug); overload;
    constructor Create(aLogAppenders: TLogAppenderList; aLogLevel: TLogType = TLogType.Debug); overload;
    destructor Destroy; override;
    procedure Debug(const aMessage: string; const aTag: string); overload;
    procedure Debug(const aMessage: string; const aParams: array of TVarRec; const aTag: string); overload;
    procedure DebugFmt(const aMessage: string; const aParams: array of TVarRec; const aTag: string);

    procedure Info(const aMessage: string; const aTag: string); overload;
    procedure Info(const aMessage: string; const aParams: array of TVarRec; const aTag: string); overload;
    procedure InfoFmt(const aMessage: string; const aParams: array of TVarRec; const aTag: string);

    procedure Warn(const aMessage: string; const aTag: string); overload;
    procedure Warn(const aMessage: string; const aParams: array of TVarRec; const aTag: string); overload;
    procedure WarnFmt(const aMessage: string; const aParams: array of TVarRec; const aTag: string);

    procedure Error(const aMessage: string; const aTag: string); overload;
    procedure Error(const aMessage: string; const aParams: array of TVarRec; const aTag: string); overload;
    procedure ErrorFmt(const aMessage: string; const aParams: array of TVarRec; const aTag: string);

    procedure Log(const aType: TLogType; const aMessage: string; const aTag: string); overload;
    procedure Log(const aType: TLogType; const aMessage: string; const aParams: array of const; const aTag: string); overload;
    procedure LogFmt(const aType: TLogType; const aMessage: string; const aParams: array of const; const aTag: string);
  end;

  TLoggerProAppenderBase = class abstract(TInterfacedObject, ILogAppender)
  private
    FLogLevel: TLogType;
    FEnabled: Boolean;
    FLastErrorTimeStamp: TDateTime;
  public
    constructor Create; virtual;
    procedure Setup; virtual; abstract;
    procedure WriteLog(const aLogItem: TLogItem); virtual; abstract;
    procedure TearDown; virtual; abstract;
    procedure TryToRestart(var Restarted: Boolean); virtual;
    procedure SetLogLevel(const Value: TLogType);
    function GetLogLevel: TLogType; inline;
    procedure SetLastErrorTimeStamp(const Value: TDateTime);
    function GetLastErrorTimeStamp: TDateTime;
    property LogLevel: TLogType read GetLogLevel write SetLogLevel;
  end;

  { @abstract(Builds a new ILogWriter instance. Call this global function to start logging like a pro.)
    Here's a sample unit that you can use in your code
    @longcode(#
    unit LoggerProConfig;

    interface

    uses
    LoggerPro;

    function Log: ILogWriter;

    implementation

    uses
    LoggerPro.FileAppender;

    var
    _Log: ILogWriter;

    function Log: ILogWriter;
    begin
    Result := _Log;
    end;

    initialization

    //If you need other appenders, feel free to add them here in the array
    _Log := BuildLogWriter([TLoggerProFileAppender.Create(10, 5)]);

    end.
    #)

    Add this unit to your project, then when you need to use the logger, include
    the unit and call one of the followings:
    @unorderedlist(
    @item(Log.Debug('This is a debug message', 'tag1'))
    @item(Log.Info('This is an information message', 'tag1'))
    @item(Log.Warn('This is a warning message', 'tag1'))
    @item(Log.Error('This is an error message', 'tag1'))
    )
  }

function GetDefaultFormatSettings: TFormatSettings;
function StringToLogType(const aLogType: string): TLogType;
function BuildLogWriter(aAppenders: array of ILogAppender; aEventsHandlers: TLoggerProEventsHandler = nil;
  aLogLevel: TLogType = TLogType.Debug): ILogWriter;

implementation

uses
  System.Types,
  LoggerPro.FileAppender,
  System.SyncObjs,
  System.DateUtils,
  System.IOUtils;

function GetDefaultFormatSettings: TFormatSettings;
begin
  Result.DateSeparator := '-';
  Result.TimeSeparator := ':';
  Result.ShortDateFormat := 'YYYY-MM-DD HH:NN:SS:ZZZ';
  Result.ShortTimeFormat := 'HH:NN:SS';
end;

function StringToLogType(const aLogType: string): TLogType;
var
  lLogType: string;
begin
  lLogType := LowerCase(Trim(aLogType));
  if lLogType = 'debug' then
    Exit(TLogType.Debug);
  if lLogType = 'info' then
    Exit(TLogType.Info);
  if lLogType = 'warning' then
    Exit(TLogType.Warning);
  if lLogType = 'error' then
    Exit(TLogType.Error);
  raise ELoggerPro.CreateFmt('Invalid LogType: ', [aLogType]);
end;

function BuildLogWriter(aAppenders: array of ILogAppender; aEventsHandlers: TLoggerProEventsHandler; aLogLevel: TLogType): ILogWriter;
var
  lLogAppenders: TLogAppenderList;
  lLogAppender: ILogAppender;
begin
  lLogAppenders := TLogAppenderList.Create;
  for lLogAppender in aAppenders do
  begin
    lLogAppenders.Add(lLogAppender);
  end;
  Result := TLogWriter.Create(lLogAppenders, aLogLevel);
  TLogWriter(Result).Initialize(aEventsHandlers);
end;

{ TLogger.TLogWriter }

function TLogWriter.AppendersCount: Integer;
begin
  Result := Self.FLogAppenders.Count;
end;

constructor TLogWriter.Create(aLogAppenders: TLogAppenderList; aLogLevel: TLogType);
begin
  inherited Create;
  FFreeAllowed := False;
  FLogAppenders := aLogAppenders;
  FLogLevel := aLogLevel;
end;

constructor TLogWriter.Create(aLogLevel: TLogType);
begin
  Create(TLogAppenderList.Create, aLogLevel);
end;

procedure TLogWriter.Debug(const aMessage, aTag: string);
begin
  Log(TLogType.Debug, aMessage, aTag);
end;

procedure TLogWriter.Debug(const aMessage: string; const aParams: array of TVarRec; const aTag: string);
begin
  Log(TLogType.Debug, aMessage, aParams, aTag);
end;

procedure TLogWriter.DebugFmt(const aMessage: string; const aParams: array of TVarRec; const aTag: string);
begin
  Debug(aMessage, aParams, aTag);
end;

destructor TLogWriter.Destroy;
begin
  FLoggerThread.Stop;
  FLogAppenders.Free;
  inherited;
end;

procedure TLogWriter.Error(const aMessage, aTag: string);
begin
  Log(TLogType.Error, aMessage, aTag);
end;

procedure TLogWriter.Error(const aMessage: string; const aParams: array of TVarRec; const aTag: string);
begin
  Log(TLogType.Error, aMessage, aParams, aTag);
end;

procedure TLogWriter.ErrorFmt(const aMessage: string; const aParams: array of TVarRec; const aTag: string);
begin
  Error(aMessage, aParams, aTag);
end;

function TLogWriter.GetAppenders(const Index: Integer): ILogAppender;
begin
  Result := Self.FLogAppenders[index];
end;

procedure TLogWriter.AddAppender(const aAppender: ILogAppender);
begin
  Self.FLogAppenders.Add(aAppender);
end;

procedure TLogWriter.DelAppender(const aAppender: ILogAppender);
var
  i: Integer;
begin
  i := Self.FLogAppenders.IndexOf(aAppender);
  if i >= 0 then
    Self.FLogAppenders.Delete(i);
end;

function TLogWriter.GetAppendersClassNames: TArray<string>;
var
  I: Cardinal;
begin
  TMonitor.Enter(FLogAppenders);
  try
    SetLength(Result, FLogAppenders.Count);
    for I := 0 to FLogAppenders.Count - 1 do
    begin
      Result[I] := TObject(FLogAppenders[I]).ClassName;
    end;
  finally
    TMonitor.Exit(FLogAppenders);
  end;
end;

procedure TLogWriter.Info(const aMessage, aTag: string);
begin
  Log(TLogType.Info, aMessage, aTag);
end;

procedure TLogWriter.Info(const aMessage: string; const aParams: array of TVarRec; const aTag: string);
begin
  Log(TLogType.Info, aMessage, aParams, aTag);
end;

procedure TLogWriter.InfoFmt(const aMessage: string; const aParams: array of TVarRec; const aTag: string);
begin
  Info(aMessage, aParams, aTag);
end;

procedure TLogWriter.Log(const aType: TLogType; const aMessage, aTag: string);
var
  lLogItem: TLogItem;
begin
  if aType >= FLogLevel then
  begin
    lLogItem := TLogItem.Create(aType, aMessage, aTag);
    try
      if not FLoggerThread.Enqueue(lLogItem) then
      begin
        raise ELoggerPro.Create
          ('Main logs queue is full. Hints: Are there appenders? Are these appenders fast enough considering the log item production?');
      end;
    except
      FreeAndNil(lLogItem);
      raise;
    end;
  end;
end;

procedure TLogWriter.Log(const aType: TLogType; const aMessage: string; const aParams: array of const; const aTag: string);
begin
  Log(aType, Format(aMessage, aParams), aTag);
end;

procedure TLogWriter.LogFmt(const aType: TLogType; const aMessage: string; const aParams: array of const; const aTag: string);
begin
  Log(aType, aMessage, aParams, aTag);
end;

procedure TLogWriter.Initialize(aEventsHandler: TLoggerProEventsHandler);
begin
  FLoggerThread := TLoggerThread.Create(FLogAppenders);
  FLoggerThread.EventsHandlers := aEventsHandler;
  FLoggerThread.Start;
end;

procedure TLogWriter.Warn(const aMessage, aTag: string);
begin
  Log(TLogType.Warning, aMessage, aTag);
end;

procedure TLogWriter.Warn(const aMessage: string; const aParams: array of TVarRec; const aTag: string);
begin
  Log(TLogType.Warning, aMessage, aParams, aTag);
end;

procedure TLogWriter.WarnFmt(const aMessage: string; const aParams: array of TVarRec; const aTag: string);
begin
  Warn(aMessage, aParams, aTag);
end;

{ TLogger.TLogItem }

function TLogItem.Clone: TLogItem;
begin
  Result := TLogItem.Create(FType, FMessage, FTag, FTimeStamp, FThreadID);
end;

constructor TLogItem.Create(const aType: TLogType; const aMessage, aTag: string);
begin
  Create(aType, aMessage, aTag, now, TThread.CurrentThread.ThreadID);
end;

constructor TLogItem.Create(const aType: TLogType; const aMessage, aTag: string; const aTimeStamp: TDateTime; const aThreadID: TThreadID);
begin
  inherited Create;
  FType := aType;
  FMessage := aMessage;
  FTag := aTag;
  FTimeStamp := aTimeStamp;
  FThreadID := aThreadID;
end;

function TLogItem.GetLogTypeAsString: string;
begin
  case FType of
    TLogType.Debug:
      Exit('DEBUG');
    TLogType.Info:
      Exit('INFO');
    TLogType.Warning:
      Exit('WARNING');
    TLogType.Error:
      Exit('ERROR');
  else
    raise ELoggerPro.Create('Invalid LogType');
  end;
end;

{ TLoggerProAppenderBase }

constructor TLoggerProAppenderBase.Create;
begin
  inherited;
  Self.FEnabled := true;
  Self.FLogLevel := TLogType.Debug;
end;

function TLoggerProAppenderBase.GetLastErrorTimeStamp: TDateTime;
begin
  Result := FLastErrorTimeStamp;
end;

function TLoggerProAppenderBase.GetLogLevel: TLogType;
begin
  Result := FLogLevel;
end;

procedure TLoggerProAppenderBase.SetLastErrorTimeStamp(const Value: TDateTime);
begin
  FLastErrorTimeStamp := Value;
end;

procedure TLoggerProAppenderBase.SetLogLevel(const Value: TLogType);
begin
  FLogLevel := Value;
end;

procedure TLoggerProAppenderBase.TryToRestart(var Restarted: Boolean);
begin
  Restarted := False;
  // do nothing "smart" here... descendant must implement specific "restart" strategies
end;

{ TLoggerProInterfacedObject }

function TLoggerProInterfacedObject._AddRef: Integer;
begin
  Result := inherited;
end;

function TLoggerProInterfacedObject._Release: Integer;
begin
  Result := inherited;
end;

{ TLoggerThread }

constructor TLoggerThread.Create(aAppenders: TLogAppenderList);
var
  LLogAppender: ILogAppender;
begin
  inherited Create(True, DefaultLoggerProMainQueueSize);

  // build appender decorators list
  FAppenderList := TObjectList<TAppenderThread>.Create(False);
  try
    for LLogAppender in aAppenders do
      FAppenderList.Add(TAppenderThread.Create(LLogAppender));
  except
    FAppenderList.Free;
    raise;
  end;
end;

procedure TLoggerThread.DoOnAppenderError(const FailAppenderClassName: string;
  const aFailedLogItem: TLogItem; const aReason: TLogErrorReason;
  var aAction: TLogErrorAction);
begin
  if Assigned(FEventsHandlers) and (Assigned(FEventsHandlers.OnAppenderError)) then
    FEventsHandlers.OnAppenderError(FailAppenderClassName, aFailedLogItem, aReason, aAction);
end;

procedure TLoggerThread.DoOnNewItem(const AItem: TLogItem);
var
  LAction: TLogErrorAction;
  LAppender: TAppenderThread;
begin
  for LAppender in FAppenderList do
  begin
    if AItem.LogType >= LAppender.GetLogLevel then
    begin
      if not LAppender.Enqueue(AItem.Clone) then
      begin
        LAction := TLogErrorAction.SkipNewest; // default
        DoOnAppenderError(TObject(LAppender.FLogAppender).ClassName, AItem,
          TLogErrorReason.QueueFull, LAction);
        case lAction of
          TLogErrorAction.SkipNewest:
            begin
              // just skip the new message
            end;
          TLogErrorAction.DiscardOlder:
            begin
              // just ignore this item
            end;
        end;
      end;
    end;
  end;
end;

procedure TLoggerThread.DoTerminate;
var
  LAppender: TAppenderThread;
begin
  // stop the appenders threads
  for LAppender in FAppenderList do
    LAppender.Stop;

  FAppenderList.Free;
  inherited;
end;

procedure TLoggerThread.SetEventsHandlers(const Value: TLoggerProEventsHandler);
begin
  FEventsHandlers := Value;
end;

{ TAppenderThread }

constructor TAppenderThread.Create(ALogAppender: ILogAppender);
begin
  inherited Create(False, DefaultLoggerProAppenderQueueSize);
  FLogAppender := ALogAppender;
end;

procedure TAppenderThread.DoAfterExecute;
begin
  FLogAppender.TearDown;
end;

procedure TAppenderThread.DoBeforeExecute;
begin
  FSetupFailCount := 0;
  FStatus := TAppenderStatus.BeforeSetup;
end;

procedure TAppenderThread.DoOnNewItem(const AItem: TLogItem);
begin
  try
    FLogAppender.WriteLog(AItem);
  except
    FLogAppender.LastErrorTimeStamp := Now;
    FStatus := TAppenderStatus.WaitAfterFail;
  end;
end;

function TAppenderThread.GetLogLevel: TLogType;
begin
  Result := FLogAppender.GetLogLevel;
end;

procedure TAppenderThread.Process(var ADoPause: Boolean);
var
  LRestarted: Boolean;
begin
  try
    // run the state machine
    ADoPause := False;

    // this state machine handles the status of the appender
    case FStatus of
      TAppenderStatus.BeforeSetup:
        begin
          try
            FLogAppender.Setup;
            FStatus := TAppenderStatus.Running;
          except
            if FSetupFailCount >= 10 then
            begin
              FStatus := TAppenderStatus.WaitAfterFail;
            end
            else
            begin
              Inc(FSetupFailCount);
              Sleep(1000); // wait before next setup call
            end;
          end;
        end;

      TAppenderStatus.ToRestart:
        begin
          try
            LRestarted := False;
            FLogAppender.TryToRestart(LRestarted);
            if LRestarted then
            begin
              FStatus := TAppenderStatus.Running;
              FLogAppender.LastErrorTimeStamp := 0;
            end
            else
            begin
              LRestarted := False;
              FLogAppender.LastErrorTimeStamp := Now;
              FStatus := TAppenderStatus.WaitAfterFail;
            end;
          except
            LRestarted := False;
          end;
          // Failing := not lRestarted;
        end;

      TAppenderStatus.WaitAfterFail:
        begin
          Sleep(500);
          if SecondsBetween(Now, FLogAppender.LastErrorTimeStamp) >= 5 then
            FStatus := TAppenderStatus.ToRestart;
        end;

      TAppenderStatus.Running:
        begin
          // dequeue the next item(s), it will trigger DoOnNewItem()
          inherited;

          // pause if successfully dequeued and processed
          ADoPause := FStatus = TAppenderStatus.Running;
        end;
    end;
  except
    // something wrong... but we cannot stop the thread. Let's retry.
  end;
end;

end.

