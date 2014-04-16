namespace HyperF

type Service<'Req, 'Res> = 'Req -> Async<'Res>

type Filter<'Req, 'ReqInner, 'ResInner, 'Res> = 'Req -> Service<'ReqInner, 'ResInner> -> Async<'Res>

type Filter<'Req, 'Res> = 'Req -> Service<'Req, 'Res> -> Async<'Res>

module Filter =

    open Async

    let identity : Filter<_,_,_,_> = fun (req:'Req) (service:Service<_,_>) -> service req

    let andThen (f2:Filter<_,_,_,_>) (f1:Filter<_,_,_,_>) : Filter<_,_,_,_> = fun req -> Continuation.bind (f1 req) f2

    let fromMaps mapReq mapRes : Filter<_,_,_,_> =
        fun (req:'Req) (service:Service<'Req2, 'Res>) -> async {
            let! req' = mapReq req
            let! res = service req'
            return! mapRes res }
        
    let fromMapReq mapReq : Filter<_,_,_,_> = fun req (service:Service<_,_>) -> mapReq req >>= (fun req -> service req)

    let fromMapReqSync mapReq : Filter<_,_,_,_> = fun req (service:Service<_,_>) -> req |> mapReq |> service

    let fromMapRes mapRes : Filter<_,_,_,_> = fun req (service:Service<_,_>) -> service req >>= (fun res -> mapRes res)
        
    let beforeAfterSync (before:'Req -> unit) (after:('Req * 'Res) -> unit) : Filter<_,_,_,_> =
        fun req (service:Service<_,_>) -> async { 
            do before req
            let! res = service req
            do after (req,res)
            return res }                       

    let timeOut (ts:System.TimeSpan) : Filter<_,_,_,_> = fun req (service:Service<_,_>) -> req |> service |> Async.timeoutAfter ts

    let timeOutMs ms = timeOut (System.TimeSpan.FromMilliseconds(ms))

    let printBeforeAfterTag : Filter<_,_,_,_> = fun req service -> beforeAfterSync (fun _ -> printfn "before") (fun _ -> printfn "after") req service

    let toService (service:Service<_,_>) (filter:Filter<_,_,_,_>) : Service<_,_> = fun req -> filter req service

    let handleEx succf errf req (service:Service<_,_>) = async {
            try
                let! res = service req
                return succf res
            with 
                exn -> return errf exn }

    let handleExUnit req service = handleEx id id req service

    let handleExEither req service = handleEx Choice1Of2 Choice2Of2 req service