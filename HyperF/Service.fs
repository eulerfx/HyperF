namespace HyperF


type Service<'TRequest, 'TResponse> = 'TRequest -> Async<'TResponse>

type Filter<'TRequest, 'TResponse> = 'TRequest -> Service<'TRequest, 'TResponse> -> Async<'TResponse>


module Filters =

    let identity req (service:Service<_,_>) = service req

    let andThen (f2:Filter<_,_>) (f1:Filter<_,_>) req = Cont.bind (f1 req) f2

    let request (filter:Filter<_,_>) (service:Service<_,_>) = fun req -> filter req service

    let ofEndo (endo:'a -> 'a) (req:'a) (service:Service<_,_>) = endo req |> service        

    let printfnF req (service:Service<_,_>) =
        printfn "before"
        let r = req |> service
        printfn "after"
        r

    let sleepBefore ms req (service:Service<_,_>) = async {
        do! Async.Sleep(ms)
        return! service req }

    let timeOut ts req (service:Service<_,_>) = req |> service |> Futures.withTimeout ts

    let timeOutMs ms = timeOut (System.TimeSpan.FromMilliseconds(ms))


module Services =
    
    let lift (service:'a -> 'b) = service >> Async.unit

    let andThen (service:Service<_,_>) (filter:Filter<_,_>) = fun req -> service |> (filter req)