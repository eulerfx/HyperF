namespace HyperF

open HyperF

type AsyncProcess<'a, 'b> = Async<AsyncStep<'a, 'b>>

and AsyncStep<'a, 'b> =
    | Stop
    | Emit of 'b * AsyncProcess<'a, 'b>
    | Await of ('a  -> AsyncProcess<'a, 'b>)


type Source<'a> = AsyncProcess<unit, 'a>

type Sink<'a> = AsyncProcess<'a, unit>


module AsyncProcess =
    
    [<GeneralizableValue>]
    let stop<'a, 'b> : AsyncProcess<'a, 'b> = Stop |> async.Return

    let emit b tp : AsyncProcess<'a, 'b> = Emit(b, tp) |> async.Return

    let emitOne b : AsyncProcess<'a, 'b> = emit b stop

    let await (f:'a -> AsyncProcess<'a, 'b>) = Await f |> async.Return

    let rec map (f:'b -> 'c) (p:AsyncProcess<'a, 'b>) : AsyncProcess<'a, 'c> = async {
        let! p = p
        match p with
        | Stop -> return Stop
        | Emit (h,t) -> return Emit (f h, t |> map f)
        | Await recv -> return Await (recv >> map f) }

    let rec append (p1:AsyncProcess<'a, 'b>) (p2:AsyncProcess<'a, 'b>) : AsyncProcess<'a, 'b> = async {
        let! p1 = p1
        match p1 with
        | Stop -> return! p2
        | Emit (h,t) -> return Emit (h, append t p2)
        | Await recv -> return Await (fun a -> append (recv a) p2) }    
           
    let rec bind (f:'b -> AsyncProcess<'a, 'c>) (p:AsyncProcess<'a, 'b>) : AsyncProcess<'a, 'c> = async {
        let! p = p
        match p with
        | Stop -> return Stop
        | Emit (h,t) -> return! append (f h) (bind f t)
        | Await recv -> return Await (recv >> (bind f)) }

    let feed a (p:AsyncProcess<'a, 'b>) : AsyncProcess<'a, 'b> = async {
        let! p = p
        match p with
        | Stop -> return Stop
        | Emit (h,t) -> return p
        | Await recv -> return! recv a }

    let rec feedAsyncSeq (s:AsyncSeq<'a>) (p:AsyncProcess<'a, 'b>) : AsyncProcess<'a, 'b> = async {
        let! p = p
        match p with
        | Stop -> return Stop
        | Emit (h,t) -> return Emit(h, feedAsyncSeq s t)
        | Await recv -> 
            let! s = s
            match s with
            | Nil -> return Stop
            | Cons (h,t) -> return! feedAsyncSeq t (recv h) }

    let rec applyAsyncSeq (s:AsyncSeq<'a>) (p:AsyncProcess<'a, 'b>) : AsyncSeq<'b> = async {
        let! p = p
        match p with
        | Stop -> return Nil
        | Emit (h,t) -> 
            return! asyncSeq {
                yield h
                yield! applyAsyncSeq s t }
        | Await recv -> 
            let! s = s
            match s with
            | Nil -> return Nil
            | Cons (h,t) -> return! applyAsyncSeq t (recv h) }

    let repeat (p:AsyncProcess<'a, 'b>) : AsyncProcess<'a, 'b> =
        let rec loop p' = async {
            let! p' = p'
            match p' with
            | Stop -> return! loop p
            | Emit (h,t) -> return Emit (h, loop t)
            | Await recv -> return Await (recv >> loop) }
        loop p

    let lift (f:'a -> 'b) : AsyncProcess<'a, 'b> = Await (f >> emitOne) |> async.Return |> repeat

    [<GeneralizableValue>]
    let id<'a> : AsyncProcess<'a, 'a> = lift id

    let liftAsync (f:'a -> Async<'b>) : AsyncProcess<'a, 'b> = Await (f >> Async.bind (emitOne >> repeat)) |> async.Return

    let rec sink (f:'a -> Async<unit>) : Sink<'a> = Await (f >> Async.bind (fun _ -> sink f)) |> async.Return

    let rec pipe (p1:AsyncProcess<'a, 'b>) (p2:AsyncProcess<'b, 'c>) : AsyncProcess<'a, 'c> = async {
        let! p2 = p2
        match p2 with
        | Stop -> return Stop
        | Emit (h,t) -> return Emit (h, pipe p1 t)
        | Await recv ->
            let! p1 = p1
            match p1 with
            | Stop -> return Stop
            | Emit (h,t) -> return! pipe t (recv h)
            | Await recv2 -> return Await (fun a -> pipe (recv2 a) (async.Return p2)) }


    type AsyncProcessBuilder() =
        member x.Yield(b) = emitOne b
        member x.Return(()) = stop
        member x.YieldFrom(s) = s
        member x.Zero() = stop
        member x.Bind (inp:Async<'a>, body:'a -> AsyncProcess<'a, 'b>) : AsyncProcess<'a, 'b> = async.Bind(inp, body)
        member x.Combine (p1:AsyncProcess<'a, 'b>, p2:AsyncProcess<'a, 'b>) = append p1 p2
        member x.While (gd, p:AsyncProcess<'a, 'b>) = 
            if gd() then x.Combine(p, x.Delay(fun() -> x.While(gd, p))) 
            else x.Zero()
        member x.Delay (f:unit -> AsyncProcess<'a, 'b>) = async.Delay(f)
            
    let asyncProc = new AsyncProcessBuilder()    


    let rec loop (s:'s) (f:'a -> 's -> 'b * 's) : AsyncProcess<'a, 'b> =
        await (fun a -> let b,s' = f a s in emit b (loop s' f))

    [<GeneralizableValue>]
    let count<'a> : AsyncProcess<'a, int> = loop 0 (fun _ c -> let c' = c + 1 in c',c')

    let sum : AsyncProcess<_, _> = 
        let rec go acc = 
            await (fun x -> let sum = x + acc in emit sum (go sum))
        go 0
    
    let map2 (p1:AsyncProcess<'a, 'b>) (p2:AsyncProcess<'a, 'c>) (f:'b -> 'c -> 'd) : AsyncProcess<'a, 'd> =
        p1 |> bind (fun b -> p2 |> map (fun c -> f b c))

    let zip (p1:AsyncProcess<'a, 'b>) (p2:AsyncProcess<'a, 'c>) : AsyncProcess<'a, 'b * 'c> = map2 p1 p2 (fun b c -> b,c)

    let zipWithIndex (p:AsyncProcess<'a, 'b>) : AsyncProcess<'a, 'b * int> = zip p count

    let rec applyList (ls:list<'a>) (p:AsyncProcess<'a, 'b>) = async {
        let! p = p
        match p with
        | Stop -> return List.empty
        | Emit (h,t) -> return! (applyList ls t) |> Async.map (fun ls -> h::ls)
        | Await recv ->
            match ls with
            | hd::tl -> return! applyList tl (recv hd)
            | _ -> return List.empty }

    let rec ofAsyncSeq (s:AsyncSeq<'a>) : AsyncProcess<_, 'a> = async {
        let! s = s
        match s with
        | Nil -> return Stop
        | Cons (h,t) -> return Emit (h, ofAsyncSeq t) }

    let rec toAsyncSeq (p:AsyncProcess<unit, 'a>) : AsyncSeq<'a> = async {
        let! p = p
        match p with
        | Stop -> return Nil
        | Emit (h,t) -> return! asyncSeq { yield h ; yield! toAsyncSeq t }
        | Await recv -> return! toAsyncSeq (recv()) }