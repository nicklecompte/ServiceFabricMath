module ServiceFabricMath.Concurrency.ActorInterfaces

type IJob = interface end

type IResult = interface end

type JobToken<'TResult> =
    | Failed of System.Exception
    | InProgress
    | Completed of 'TResult
    
type IWorkerActor<'TJob,'TResult when 'TJob :> IJob> =
    abstract member ReceiveJob: 'TJob -> JobToken<'TResult>
    abstract member BroadcastJobStatus : unit -> JobToken<'TResult>

type IJobGiverService<'TWorker,'TJob,'TResult when
    'TWorker :> IWorkerActor<'TJob,'TResult> and
    'TJob :> IJob> =
    abstract member GiveWork : 'TJob -> 'TWorker -> JobToken<'TResult>