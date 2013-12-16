namespace HyperF


type Service<'TRequest, 'TResponse> = 'TRequest -> Async<'TResponse>

type Filter<'TRequest, 'TResponse> = 'TRequest -> Service<'TRequest, 'TResponse> -> Async<'TResponse>


module Filters =

    open Async

    let identity req (service:Service<_,_>) = service req

    let andThen (f2:Filter<_,_>) (f1:Filter<_,_>) : Filter<_,_> = fun req -> Cont.bind (f1 req) f2

    //let map f req (service:Service<_,_>) = f req |> service

    let before before req (service:Service<_,_>) = before req >>= (fun () -> service req)

    let after after req (service:Service<_,_>) = async {
        let! res = service req
        do! after (req,res)
        return res }

    let beforeAfter before after req (service:Service<_,_>) = async { 
        do! before req
        let! res = service req
        do! after (req,res)
        return res }
        
    let private beforeAfterSync before after req (service:Service<_,_>) = async { 
        do before req
        let! res = service req
        do after (req,res)
        return res }                       

    let timeOut ts req (service:Service<_,_>) = req |> service |> Futures.withTimeout ts

    let timeOutMs ms = timeOut (System.TimeSpan.FromMilliseconds(ms))

    let printfnF req service = beforeAfterSync (fun _ -> printfn "before") (fun _ -> printfn "after") req service


module Services =
    
    let lift (service:'a -> 'b) = service >> Async.unit

    let andThen (service:Service<_,_>) (filter:Filter<_,_>) = fun req -> service |> (filter req)