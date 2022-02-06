// ***************************************************************************
// Copyright (c) 2021 Vitaly Bondaryuk
// This code is licensed under MIT license
// ***************************************************************************

unit ProcessingThread;

interface

uses
  System.Classes
, System.SyncObjs
, System.Generics.Collections
;

type
  /// <summary>
  /// Thread safe queue, based on TObjectQueue
  /// </summary>
  TThreadSafeQueue<T: class> = class
  strict private
    FQueue: TObjectQueue<T>;
  private
    FMaxSize: Integer;
  protected
    function  Lock: TObjectQueue<T>; inline;
    procedure Unlock; inline;
  public
    constructor Create(const AMaxSize: Integer = 0); virtual;
    destructor Destroy; override;
    function Enqueue(const AItem: T): Boolean;
    function Dequeue: T; overload;
    property MaxSize: Integer read FMaxSize write FMaxSize;
  end;

  TProcessingThread = class abstract(TThread)
  private
    FWakeEvent: TEvent;
    FStopEvent: TEvent;
  protected
    procedure DoBeforeExecute; virtual;
    procedure DoAfterExecute; virtual;
    procedure DoTerminate; override;
    procedure Execute; override;
    procedure Process(var ADoPause: Boolean); virtual; abstract;
  public
    constructor Create(CreateSuspended: Boolean);
    procedure Pause; inline;
    procedure UnPause; inline;
    procedure Stop;
  end;

  TProcessingQueueThread<T: class> = class abstract(TProcessingThread)
  private
    FQueue: TThreadSafeQueue<T>;
  protected
    procedure DoOnNewItem(const AItem: T); virtual; abstract;
    procedure DoTerminate; override;
    procedure Process(var ADoPause: Boolean); override;
  public
    constructor Create(CreateSuspended: Boolean; const AMaxQueueSize: Integer = 0);
    function Enqueue(AItem: T): Boolean;
  end;

implementation

{ TThreadSafeQueue<T> }

constructor TThreadSafeQueue<T>.Create(const AMaxSize: Integer);
begin
  FQueue := TObjectQueue<T>.Create;
  FMaxSize := AMaxSize;
end;

destructor TThreadSafeQueue<T>.Destroy;
begin
  Lock;
  // clean the queue
  try
    while FQueue.Count > 0 do
      FQueue.Dequeue;
  finally
    Unlock;
  end;

  FQueue.Free;
  inherited;
end;

function TThreadSafeQueue<T>.Dequeue: T;
begin
  Lock;
  try
    if FQueue.Count > 0 then
      Result := FQueue.Extract
    else
      Result := nil;
  finally
    Unlock;
  end;
end;

function TThreadSafeQueue<T>.Enqueue(const AItem: T): Boolean;
begin
  Lock;
  try
    Result := (FMaxSize = 0) or (FQueue.Count < FMaxSize);
    if Result then
      FQueue.Enqueue(AItem);
  finally
    Unlock;
  end;
end;

function TThreadSafeQueue<T>.Lock: TObjectQueue<T>;
begin
  MonitorEnter(FQueue);
  Result := FQueue;
end;

procedure TThreadSafeQueue<T>.Unlock;
begin
  MonitorExit(FQueue);
end;

{ TProcessingThread }

constructor TProcessingThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);

  // free on terminate, after Stop()
  FreeOnTerminate := True;

  // Wake event, used to awake the thread
  FWakeEvent := TEvent.Create(nil, True, not CreateSuspended, '');
  // Stop event, used to stop and terminate the thread
  FStopEvent := TEvent.Create(nil, True, False, '');
end;

procedure TProcessingThread.DoAfterExecute;
begin
  // override in descendant class if needed
end;

procedure TProcessingThread.DoBeforeExecute;
begin
  // override in descendant class if needed
end;

procedure TProcessingThread.DoTerminate;
begin
  inherited;

  FWakeEvent.Free;
  FStopEvent.Free;
end;

procedure TProcessingThread.Execute;
var
  LDoPause: Boolean;
begin
  DoBeforeExecute;
  while not Terminated do
  begin
    if FWakeEvent.WaitFor = wrSignaled then
    begin
      // check stop event first
      if FStopEvent.WaitFor(0) = wrSignaled then
        Break;

      // if nothing is set in Process() then pause the thread
      LDoPause := True;

      // do processing
      Process(LDoPause);

      // TODO: Add exception handling

      if LDoPause then
        Pause;
    end;
  end;
  DoAfterExecute;
end;

procedure TProcessingThread.Pause;
begin
  FWakeEvent.ResetEvent;
end;

procedure TProcessingThread.Stop;
begin
  // set Stop event first and then Awake the thread
  FStopEvent.SetEvent;
  FWakeEvent.SetEvent;
end;

procedure TProcessingThread.UnPause;
begin
  FWakeEvent.SetEvent;
end;

{ TProcessingQueueThread<T> }

constructor TProcessingQueueThread<T>.Create(CreateSuspended: Boolean; const AMaxQueueSize: Integer);
begin
  inherited Create(CreateSuspended);

  // Message queue
  FQueue := TThreadSafeQueue<T>.Create(AMaxQueueSize);
end;

procedure TProcessingQueueThread<T>.DoTerminate;
begin
  inherited;

  FQueue.Free;
end;

function TProcessingQueueThread<T>.Enqueue(AItem: T): Boolean;
begin
  if Terminated then
    Exit;

  Result := FQueue.Enqueue(AItem);
  if Result then
    UnPause;
end;

procedure TProcessingQueueThread<T>.Process(var ADoPause: Boolean);
var
  LItem: T;
begin
  // process all queued messages
  repeat
    LItem := FQueue.Dequeue;
    if LItem <> nil then
    begin
      try
        DoOnNewItem(LItem);
      finally
        LItem.Free;
      end;
    end;
  until LItem = nil;
end;

end.
