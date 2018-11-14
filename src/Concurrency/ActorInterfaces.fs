module ServiceFabricMath.Concurrency.ActorInterfaces

type IJob = interface end

type IResult = interface end

type JobToken<'TResult> =
    | Failed of System.Exception
    | InProgress
    | Completed of 'TResult
    
type IWorkerActor<'TResult> =
    abstract member ReceiveJob: IJob -> JobToken<'TResult>