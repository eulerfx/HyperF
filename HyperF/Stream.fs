namespace HyperF

type Process<'a, 'b> =
    | Halt
    | Emit of head:seq<'b> * tail:Process<'a, 'b>
    | Await of req:Async<'a> * recv:('a -> Process<'a, 'b>) * fallback:Process<'a, 'b>    

module Process =
    
    let rec map (f:'b -> 'c) = function
        | Halt -> Halt
        | Emit (h,t) -> Emit (Seq.map f h, map f t) 
        | Await (req,recv,fallback) -> Await (req, recv >> map f, map f fallback)

    let emitSeq (ss:seq<'b>) = function
        | Emit (h,t) -> Emit (Seq.append ss h, t)
        | p -> Emit (ss,p)

    let emit a = emitSeq (Seq.singleton a) 

    let emitOne a = emit a Halt

    let emitLazy (a:Lazy<'a>) = Emit (Seq.delay (fun () -> Seq.singleton (a.Force())), Halt)

    let awaitHalt (req:Async<'a>) (recv:'a -> Process<'a, 'b>) = Await (req, recv, Halt)

    let rec append (p1:Process<'a, 'b>) (p2:Process<'a, 'b>) : Process<'a, 'b> =
        match p1 with
        | Halt -> p2
        | Emit (h,t) -> emitSeq h (append t p2)
        | Await (req,recv,fb) -> Await (req, recv >> (append p2), append fb p2)

    let inline (++) p1 p2 = append p1 p2

    let rec bind (f:'b -> Process<'a, 'c>) = function
        | Halt -> Halt
        | Emit (h,t) -> 
            if (h |> Seq.isEmpty) then bind f t
            else (f (Seq.nth 0 h)) ++ (bind f (emitSeq (Seq.skip 1 h) t)) 
        | Await (req,recv,fb) -> Await(req, recv >> (bind f), bind f fb)

    let rec applyList (ls:list<'a>) (p:Process<'a, 'b>) =
        match p with
        | Halt -> List.empty
        | Emit (h,t) -> (h |> List.ofSeq) @ applyList ls t
        | Await (_,recv,fb) ->
            match ls with
            | hd::tl -> recv hd |> applyList tl 
            | ls -> applyList ls fb

    let rec feed (ss:seq<'a>) (p:Process<'a, 'b>) =
        let rec go ss p =
            match p with
            | Halt -> Halt
            | Emit (h,t) -> Emit (h, feed ss t)
            | Await (_,recv,_) ->
                if (ss |> Seq.isEmpty) then p
                else go (Seq.skip 1 ss) (recv (Seq.nth 0 ss))
        go ss p

    let rec pipe (p1:Process<'a, 'b>) (p2:Process<'b, 'c>) : Process<'a, 'c> =
        match p2 with
        | Halt -> Halt
        | Emit (h,t) -> Emit (h, pipe p1 t)
        | Await (req,recv,fb) ->
            match p1 with
            | Halt -> pipe Halt fb
            | Emit (h,t) -> pipe t (feed h p2)
            | Await (req',recv',fb') -> Await (req', (fun a -> pipe (recv' a) p2), (pipe fb' fb))

    let inline (==>) p1 p2 = pipe p1 p2

    let rec lift (req:Async<'a>) (f:'a -> 'b) : Process<'a, 'b> = awaitHalt req (fun a -> emit (f a) (lift req f))

    let repeat (p:Process<'a, 'b>) : Process<'a, 'b> =
        let rec loop = function
            | Halt -> loop p
            | Await (req,recv,fb) -> Await (req, recv >> loop, fb)
            | Emit (h,t) -> Emit (h, loop t)
        loop p
       
//    let filter (pred:'a -> bool) : Process<'a, 'a> =
//        await1 ((fun a -> if pred a then emitOne a else Halt),Halt) |> repeat





/// An IO state machine.
type Machine<'a, 'b> =
    | Stop
    | Emit of head:seq<'b> * tail:Machine<'a, 'b>
    | Yield of Async<Machine<'a, 'b>>

module Machine =
    
    let emit s = Emit(s, Stop)

    let unit a = emit (Seq.singleton a)

    let rec append (p1:Machine<'a, 'b>) (p2:Machine<'a, 'b>) =
        match p1 with
        | Stop -> p2
        | Emit (h,t) -> Emit (h, append t p2)
        | Yield ap -> Yield (ap |> Async.map (append p2))

    let rec map (f:'b -> 'c) = function
        | Stop -> Stop
        | Emit (h,t) -> Emit (Seq.map f h, map f t)
        | Yield ap -> Yield (Async.map (map f) ap)
           
    let rec bind (f:'b -> Machine<'a, 'c>) = function
        | Stop -> Stop
        | Emit (h,t) -> 
            if (Seq.isEmpty h) then bind f t
            else f (Seq.nth 0 h)
        | Yield ap -> Yield (Async.map (bind f) ap)

    