namespace HyperF

[<AutoOpen>]
module AsyncEx =

    open System
    open System.Threading
    open System.Threading.Tasks    

    type Async with
    
        static member returnA a = async { return a }

        static member parAp (f:Async<'a -> 'b>) (a:Async<'a>) = async {
            let! f = Async.StartChild f
            let! a = Async.StartChild a
            let! f = f
            let! a = a
            return f a                
        }

        static member map f a = async {
            let! a = a
            return f a }

        static member bind a k = async {
            let! a = a
            return! k a }

        static member timeoutAfter (timeout:TimeSpan) (c:Async<'a>) = async {
            let! r = Async.StartChild(c, (int)timeout.TotalMilliseconds)
            return! r }

