namespace HyperF

type Process<'In, 'Out> =
    | Emit of head:seq<'Out> * tail:Process<'In, 'Out>
    | Await of recv:('In -> Process<'In, 'Out>) * fallback:Process<'In, 'Out>
    | Halt

module Process =
    
    let rec map f = function
        | Halt -> Halt
        | Emit (h,t) -> Emit (h |> Seq.map f, t |> map f) 
        | Await (recv,fallback) -> Await (recv >> map f, fallback |> map f)

    let inline emitAll p xs = 
        match p with
        | Emit (h2,tp) -> Emit (Seq.append h2 xs, tp)
        | p -> Emit (xs,p)

    let emit a = Emit (Seq.singleton a, Halt)

    let emitLazy (a:Lazy<'a>) = Emit (Seq.delay (fun() -> Seq.singleton (a.Force())), Halt)

    let rec append p1 p2 =
        match p1 with
        | Halt -> p2
        | Emit (h,t) -> emitAll (append t p2) h
        | Await (r,fb) -> Await(r >> (append p2), append fb p2) 

    let rec flatMap f = function
        | Halt -> Halt
        | Emit (h,t) -> 
            if (h |> Seq.isEmpty) then t |> flatMap f
            else append (f h |> Seq.nth 0) (emitAll t (Seq.skip 1 h))
        | Await (r,fb) -> Await(r >> (flatMap f), flatMap f fb)

    let rec applyList p (ls:'a list) = 
        match p with
        | Halt -> List.empty
        | Emit (h,tp) -> (h |> List.ofSeq) @ applyList tp ls
        | Await (recv,fb) -> 
            match ls with
            | hd::tl -> applyList (recv hd) tl
            | ls -> applyList fb ls