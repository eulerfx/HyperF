namespace HyperF

type Machine<'a, 'b> =
    | Halt
    | Emit of head:seq<'b> * tail:Machine<'a, 'b>
    | Await of req:Async<'a> option * recv:('a -> Machine<'a, 'b>) * fallback:Machine<'a, 'b>    

module Machine =
    
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

    let awaitHalt (req:Async<'a> option) (recv:'a -> Machine<'a, 'b>) = Await (req, recv, Halt)

    let rec append (p1:Machine<'a, 'b>) (p2:Machine<'a, 'b>) : Machine<'a, 'b> =
        match p1 with
        | Halt -> p2
        | Emit (h,t) -> emitSeq h (append t p2)
        | Await (req,recv,fb) -> Await (req, recv >> (append p2), append fb p2)

    let inline (++) p1 p2 = append p1 p2

    let rec bind (f:'b -> Machine<'a, 'c>) = function
        | Halt -> Halt
        | Emit (h,t) -> 
            if (h |> Seq.isEmpty) then bind f t
            else (f (Seq.nth 0 h)) ++ (bind f (emitSeq (Seq.skip 1 h) t)) 
        | Await (req,recv,fb) -> Await(req, recv >> (bind f), bind f fb)

    let rec applyList (ls:list<'a>) (p:Machine<'a, 'b>) =
        match p with
        | Halt -> List.empty
        | Emit (h,t) -> (h |> List.ofSeq) @ applyList ls t
        | Await (_,recv,fb) ->
            match ls with
            | hd::tl -> recv hd |> applyList tl 
            | ls -> applyList ls fb

    let rec feed (ss:seq<'a>) (p:Machine<'a, 'b>) =
        let rec go ss p =
            match p with
            | Halt -> Halt
            | Emit (h,t) -> Emit (h, feed ss t)
            | Await (_,recv,_) ->
                if (ss |> Seq.isEmpty) then p
                else go (Seq.skip 1 ss) (recv (Seq.nth 0 ss))
        go ss p

    let rec pipe (p1:Machine<'a, 'b>) (p2:Machine<'b, 'c>) : Machine<'a, 'c> =
        match p2 with
        | Halt -> Halt
        | Emit (h,t) -> Emit (h, pipe p1 t)
        | Await (req,recv,fb) ->
            match p1 with
            | Halt -> pipe Halt fb
            | Emit (h,t) -> pipe t (feed h p2)
            | Await (req',recv',fb') -> Await (req', (fun a -> pipe (recv' a) p2), (pipe fb' fb))

    let inline (|=>) p1 p2 = pipe p1 p2

    let rec lift (f:'a -> 'b) : Machine<'a, 'b> = awaitHalt None (fun a -> emit (f a) (lift f))

    let repeat (p:Machine<'a, 'b>) : Machine<'a, 'b> =
        let rec loop = function
            | Halt -> loop p            
            | Emit (h,t) -> Emit (h, loop t)
            | Await (req,recv,fb) -> Await (req, recv >> loop, fb)
        loop p