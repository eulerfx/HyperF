namespace HyperF

//type Future = Async

//type Future<'a> = Async<'a>

[<AutoOpen>]
module Futures =

    open System
    open System.Threading
    open System.Threading.Tasks    

    // source: http://fpish.net/topic/None/75786
    let withTimeout (timeout:TimeSpan) (computation:'a Async) : 'a Async =

        let invokeOnce funcs =
            let counter = ref 0
            let invokeOnce' f x =
                if (Interlocked.CompareExchange (counter, 1, 0) = 0) then
                    f x
            let (a, b, c) = funcs
            (invokeOnce' a, invokeOnce' b, invokeOnce' c)

        let callback (success, error, cancellation) =
            let (success, error, cancellation) = invokeOnce (success, error, cancellation)
            let fetchResult = async {
                let! result = computation
                success result }
            let timeoutExpired = async {
                do! Async.Sleep (int timeout.TotalMilliseconds)
                let ex = new TimeoutException ("Timeout expired") :> Exception
                error ex }
 
            Async.StartImmediate fetchResult
            Async.StartImmediate timeoutExpired
 
        Async.FromContinuations callback

    type Async with
    
        static member unit a = async { return a }

        static member map f a = async {
            let! a = a
            return f a }

        static member flatMap a k = async {
            let! a = a
            return! k a }

        // simpler timeout impl
        static member timeoutAfter (timeoutMs:int,c:Async<_>) = async {
            let! r = Async.StartChild(c, timeoutMs)
            return! r }

