namespace HyperF

type Service<'Req, 'Res> = 'Req -> Async<'Res>

type Filter<'Req, 'Req2, 'Res2, 'Res> = 'Req -> Service<'Req2, 'Res2> -> Async<'Res>

type Filter<'Req, 'Res> = Filter<'Req, 'Req, 'Res, 'Res>

module Filter =

    open Async

    let identity req (service:Service<_,_>) = service req

    let combine (f2:Filter<_,_,_,_>) (f1:Filter<_,_,_,_>) : Filter<_,_,_,_> = fun req -> Cont.bind (f1 req) f2

    let fromMap mapReq mapRes =
        fun req (service:Service<_,_>) -> async {
            let! req' = mapReq req
            let! res = service req'
            return! mapRes res }
        
    let fromMapReq mapReq req (service:Service<_,_>) = mapReq req >>= (fun req -> service req)

    let fromMapReqSync mapReq req (service:Service<_,_>) = req |> mapReq |> service

    let fromMapRes mapRes req (service:Service<_,_>) = service req >>= (fun res -> mapRes res)
        
    let private beforeAfterSync before after req (service:Service<_,_>) = async { 
        do before req
        let! res = service req
        do after (req,res)
        return res }                       

    let timeOut ts req (service:Service<_,_>) = req |> service |> Futures.withTimeout ts

    let timeOutMs ms = timeOut (System.TimeSpan.FromMilliseconds(ms))

    let printfnF req service = beforeAfterSync (fun _ -> printfn "before") (fun _ -> printfn "after") req service

    let toService (service:Service<_,_>) (filter:Filter<_,_,_,_>) = fun req -> service |> (filter req)