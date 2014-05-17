namespace HyperF

open System.Threading

type BlockingRingBufferQueue<'a>(capacity:int) =
    
    let buffer : 'a[] = Array.zeroCreate capacity
    let wp = ref -1
    let rp = ref -1

    let readLatch = new ManualResetEventSlim()
    let writeLatch = new ManualResetEventSlim()

    let rec add a = async {
        let wp' = Interlocked.Increment(wp)
        if (wp' < capacity) then 
            buffer.[wp'] <- a
            readLatch.Set()
            return ()
        else
            
            wp := -1

            do! Async.AwaitWaitHandle(writeLatch.WaitHandle) |> Async.Ignore
            return! add a }

    let rec dequeue () = async {
        
        let rp' = 
            let rp' = Interlocked.Increment(rp)
            if (rp' >= capacity) then Interlocked.Exchange(rp, 0)
            else rp'

        if (rp' < !wp) then            
            let a = buffer.[rp']
            writeLatch.Set()
            return a
        else 
            do! Async.AwaitWaitHandle(readLatch.WaitHandle) |> Async.Ignore
            return! dequeue () }

    member this.Add(item) = add item

    member this.Take() = dequeue()