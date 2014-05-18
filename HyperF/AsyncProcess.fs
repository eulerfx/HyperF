namespace HyperF

open HyperF

/// An async transducer of input of type 'a to output of type 'b.
type AsyncProcess<'a, 'b> = Async<AsyncStep<'a, 'b>>

and AsyncStep<'a, 'b> =
    | Stop
    | Emit of 'b * AsyncProcess<'a, 'b>
    | Await of ('a  -> AsyncProcess<'a, 'b>) * AsyncProcess<'a, 'b>


type Source<'a> = AsyncProcess<unit, 'a>

type Sink<'a> = AsyncProcess<'a, unit>

type Tee<'a, 'b, 'c> = AsyncProcess<Choice<'a, 'b>, 'c>



module AsyncProcess =
    
    [<GeneralizableValue>]
    let stop<'a, 'b> : AsyncProcess<'a, 'b> = Stop |> async.Return

    let emit b tp : AsyncProcess<'a, 'b> = Emit(b, tp) |> async.Return
    
    let inline emitOne b : AsyncProcess<'a, 'b> = emit b stop

    let await (f:'a -> AsyncProcess<'a, 'b>) (t:AsyncProcess<'a, 'b>) : AsyncProcess<'a, 'b> = Await (f,t) |> async.Return

    let inline awaitStop (f:'a -> AsyncProcess<'a, 'b>) = await f stop

    let inline delay (f:unit -> AsyncProcess<'a, 'b>) : AsyncProcess<'a, 'b> = async.Delay f

    let rec map (f:'b -> 'c) (p:AsyncProcess<'a, 'b>) : AsyncProcess<'a, 'c> = async {
        let! p = p
        match p with
        | Stop -> return Stop
        | Emit (h,t) -> return Emit (f h, t |> map f)
        | Await (recv,t) -> return Await (recv >> map f, map f t) }

    let rec append (p1:AsyncProcess<'a, 'b>) (p2:AsyncProcess<'a, 'b>) : AsyncProcess<'a, 'b> = async {
        let! p1 = p1
        match p1 with
        | Stop -> return! p2
        | Emit (h,t) -> return Emit (h, append t p2)
        | Await (recv,t) -> return Await ((fun a -> append (recv a) p2), append t p2) }    
           
    let rec bind (f:'b -> AsyncProcess<'a, 'c>) (p:AsyncProcess<'a, 'b>) : AsyncProcess<'a, 'c> = async {
        let! p = p
        match p with
        | Stop -> return Stop
        | Emit (h,t) -> return! append (f h) (bind f t)
        | Await (recv,t) -> return Await (recv >> bind f, bind f t) }
    
    let inline (>>=) p f = bind f p
    let inline (=<<) f p = bind f p
    let inline (>>.) p t = p >>= (fun _ -> t)

    let rec join (p:AsyncProcess<'a, AsyncProcess<'a, 'b>>) : AsyncProcess<'a, 'b> = async {
        let! p = p
        match p with
        | Stop -> return Stop
        | Emit (h,t) -> return! append h (join t)
        | Await (recv,t) -> return! await (recv >> join) (join t) }

    let feed a (p:AsyncProcess<'a, 'b>) : AsyncProcess<'a, 'b> = async {
        let! p = p
        match p with
        | Stop -> return Stop
        | Emit (h,t) -> return p
        | Await (recv,_) -> return! recv a }

    let rec feedAsyncSeq (s:AsyncSeq<'a>) (p:AsyncProcess<'a, 'b>) : AsyncProcess<'a, 'b> = async {
        let! p = p
        match p with
        | Stop -> return Stop
        | Emit (h,t) -> return Emit(h, feedAsyncSeq s t)
        | Await (recv,t) -> 
            let! s = s
            match s with
            | Nil -> return! t
            | Cons (h,t) -> return! feedAsyncSeq t (recv h) }

    let rec applyAsyncSeq (s:AsyncSeq<'a>) (p:AsyncProcess<'a, 'b>) : AsyncSeq<'b> = async {
        let! p = p
        match p with
        | Stop -> return Nil
        | Emit (h,t) -> 
            return! asyncSeq {
                yield h
                yield! applyAsyncSeq s t }
        | Await (recv,t) -> 
            let! s = s
            match s with
            | Nil -> return! applyAsyncSeq (AsyncSeq.empty) t
            | Cons (h,t) -> return! applyAsyncSeq t (recv h) }

    let repeat (p:AsyncProcess<'a, 'b>) : AsyncProcess<'a, 'b> =
        let rec loop p' = async {
            let! p' = p'
            match p' with
            | Stop -> return! loop p
            | Emit (h,t) -> return Emit (h, loop t)
            | Await (recv,t) -> return Await (recv >> loop, t) }
        loop p

    let repeated a : Source<'a> = emitOne a |> repeat

    let echo<'a> : AsyncProcess<'a, 'a> = awaitStop emitOne |> repeat

    let lift (f:'a -> 'b) : AsyncProcess<'a, 'b> = awaitStop (f >> emitOne) |> repeat

    [<GeneralizableValue>]
    let id<'a> : AsyncProcess<'a, 'a> = lift id

    let liftAsync (f:'a -> Async<'b>) : AsyncProcess<'a, 'b> = awaitStop (f >> Async.bind (emitOne >> repeat))

    let rec sink (f:'a -> Async<unit>) : Sink<'a> = awaitStop (f >> Async.bind (fun _ -> sink f))

    let buffer (n:int) : AsyncProcess<'a, 'a list> =
        let rec go acc n =
            match acc,n with
            | [],0 -> stop
            | acc,0 -> emitOne (List.rev acc)
            | acc,n -> await (fun i -> go (i::acc) (n - 1)) (emitOne acc)
        go [] n |> repeat
                            
    let rec filter (pred:'a -> bool) (p:AsyncProcess<'a, 'b>) = awaitStop (fun a -> if pred a then emitOne a else stop)

    let rec pipe (p1:AsyncProcess<'a, 'b>) (p2:AsyncProcess<'b, 'c>) : AsyncProcess<'a, 'c> = async {
        let! p2 = p2
        match p2 with
        | Stop -> return Stop
        | Emit (h,t) -> return Emit (h, pipe p1 t)
        | Await (recv2,t2) ->
            let! p1 = p1
            match p1 with
            | Stop -> return! pipe stop t2
            | Emit (h,t) -> return! pipe t (recv2 h)
            | Await (recv,t) -> return Await ((fun a -> pipe (recv a) (async.Return p2)), pipe t t2) }

    let inline (|=>) p1 p2 = pipe p1 p2

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
        awaitStop (fun a -> let b,s' = f a s in emit b (loop s' f))

    [<GeneralizableValue>]
    let count<'a> : AsyncProcess<'a, int> = loop 0 (fun _ c -> let c' = c + 1 in c',c')
    
    let map2 (p1:AsyncProcess<'a, 'b>) (p2:AsyncProcess<'a, 'c>) (f:'b -> 'c -> 'd) : AsyncProcess<'a, 'd> =
        p1 |> bind (fun b -> p2 |> map (fun c -> f b c))

    let zip (p1:AsyncProcess<'a, 'b>) (p2:AsyncProcess<'a, 'c>) : AsyncProcess<'a, 'b * 'c> = map2 p1 p2 (fun b c -> b,c)

    let zipWithIndex (p:AsyncProcess<'a, 'b>) : AsyncProcess<'a, 'b * int> = zip p count

    let rec applyList (ls:list<'a>) (p:AsyncProcess<'a, 'b>) : Async<'b list> = async {
        let! p = p
        match p with
        | Stop -> return List.empty
        | Emit (h,t) -> return! (applyList ls t) |> Async.map (fun ls -> h::ls)
        | Await (recv,t) ->
            match ls with
            | hd::tl -> return! applyList tl (recv hd)
            | [] -> return! applyList ls t }

    let rec toSource (s:Source<'a>) (p:AsyncProcess<'a, 'b>) : Source<'b> = async {
        let! p = p
        match p with
        | Stop -> return Stop
        | Emit (h,t) -> return! emit h (toSource s t)
        | Await (recv,t) -> 
            let! s = s
            match s with
            | Stop -> return! toSource stop t            
            | Emit (h,t) -> return! toSource t (recv h)
            | Await _ -> return Stop }

    let rec toSink (sink:Sink<'b>) (p:AsyncProcess<'a, 'b>) : Sink<'a> = async {
        let! p = p
        match p with
        | Stop -> return Stop
        | Emit (h,t) -> return! let sink' = feed h sink in toSink sink' t
        | Await (recv,t) -> return! await (recv >> toSink sink) (toSink sink t) }
 
        

    //let tee (t:Tee<'b, 'c, 'd>) (p1:AsyncProcess<'a, 'b>) (p2:AsyncProcess<'a, 'c>) : AsyncProcess<'a, 'd> =


    let rec ofAsyncSeq (s:AsyncSeq<'a>) : Source<'a> = async {
        let! s = s
        match s with
        | Nil -> return Stop
        | Cons (h,t) -> return Emit (h, ofAsyncSeq t) }

    let rec toAsyncSeq (p:Source<'a>) : AsyncSeq<'a> = async {
        let! p = p
        match p with
        | Stop -> return Nil
        | Emit (h,t) -> return! asyncSeq { yield h ; yield! toAsyncSeq t }
        | Await (recv,t) -> return! AsyncSeq.append (toAsyncSeq (recv())) (toAsyncSeq t) }


    let runSource (f:'a -> Async<unit>) (s:Source<'a>) : Async<unit> = s |> toAsyncSeq |> AsyncSeq.iterAsyncPar f




type AsyncParSeq<'a> = Async<seq<Async<'a>>>

module AsyncParSeq =
    
    let unit a : AsyncParSeq<'a> = a |> async.Return |> Seq.singleton |> async.Return

    let map f (ps:AsyncParSeq<'a>) : AsyncParSeq<'b> =
        Async.map (Seq.map (Async.map f)) ps
