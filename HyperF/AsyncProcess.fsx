#load "Prelude.fs"
#load "Async.fs"
#load "AsyncSeq.fs"
#load "AsyncProcess.fs"

open HyperF



let p2 = AsyncProcess.count |> AsyncProcess.applyList ["hello";"world";"foo";"bar"] |> Async.RunSynchronously

printfn "%A" p2