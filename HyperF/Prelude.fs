namespace HyperF

[<AutoOpen>]
module Prelude =
    
    let flip f a b = f b a

    let uncurry f (a,b) = f a b

    let tuple a b = (a,b)


type Continuation<'a,'r> = ('a  -> 'r) -> 'r

module Continuation =     

    // based on Haskell implementation from "The Essence of Functional Programming" http://www.eliza.ch/doc/wadler92essence_of_FP.pdf
    let bind (m:Continuation<'a, 'r>) k c = m (fun a -> k a c)


module Strings =

    open System

    let inline equalTo (comp:StringComparison) (a:string) (b:string) = String.Compare(a, b, comp) = 0

    let equalToIgnoreCase = equalTo (StringComparison.OrdinalIgnoreCase)

    let inline startsWith (comp:StringComparison) (a:string) (b:string) = a.StartsWith(b, comp)

    let startsWithIngoreCase = startsWith (StringComparison.OrdinalIgnoreCase)

    let inline split (ss:string[]) (str:string) = str.Split(ss, StringSplitOptions.RemoveEmptyEntries)    

    //let inline splitChars (str:string) (cs:char[]) = str.Split(cs, StringSplitOptions.RemoveEmptyEntries)

    let inline splitByChar (c:char) (str:string) = str.Split(c)

    let inline splitByCharRE (c:char) (str:string) = str.Split([|c|], StringSplitOptions.RemoveEmptyEntries)

    let inline trimStart (c:char) (str:string) = str.TrimStart(c)

    let inline trimEnd (c:char) (str:string) = str.TrimEnd(c)

    let inline trim (cs) (str:string) = str.Trim(cs)
    

module Option =
    
    let join opts = opts |> Seq.where Option.isSome |> Seq.map Option.get

    let concat = function 
        | Some value ->
            match value with
            | Some value -> value |> Some
            | None -> None
        | None -> None

    let getOrElse def = function Some v -> v | None -> def


module Operators =

    let inline returnM builder x = (^M: (member Return: 'b -> 'c) (builder, x))
    
    let inline bindM builder m f = (^M: (member Bind: 'd -> ('e -> 'c) -> 'c) (builder, m, f))
    
    let inline liftM builder f m =
        let inline ret x = returnM builder (f x)
        bindM builder m ret

    let inline applyM (builder1:^M1) (builder2:^M2) f m =
        bindM builder1 f <| fun f' -> bindM builder2 m <| fun m' -> returnM builder2 (f' m') 


module Async =

    open Operators
    
    /// Sequentially compose two actions, passing any value produced by the second as an argument to the first.
    let inline bind f m = async.Bind(m,f)
    /// Inject a value into the async type
    let inline returnM x = returnM async x
    /// Sequentially compose two actions, passing any value produced by the first as an argument to the second.
    let inline (>>=) m f = bindM async m f
    /// Flipped >>=
    let inline (=<<) f m = bindM async m f
    /// Sequential application
    let inline (<*>) f m = applyM async async f m
    /// Sequential application
    let inline ap m f = f <*> m
    /// Flipped map
    let inline pipe m f = liftM async f m
    let inline pipe2 x y f = returnM f <*> x <*> y
    let inline pipe3 x y z f = returnM f <*> x <*> y <*> z
    /// Transforms an async value by using a specified mapping function.
    let inline map f m = pipe m f
    /// Promote a function to a monad/applicative, scanning the monadic/applicative arguments from left to right.
    let inline lift2 f x y = returnM f <*> x <*> y
    /// Infix map
    let inline (<!>) f m = pipe m f
    /// Sequence actions, discarding the value of the first argument.
    let inline ( *>) x y = pipe2 x y (fun _ z -> z)
    /// Sequence actions, discarding the value of the second argument.
    let inline ( <*) x y = pipe2 x y (fun z _ -> z)

    /// Sequentially compose two async actions, discarding any value produced by the first
    let inline (>>.) m f = bindM async m (fun _ -> f)
    /// Left-to-right Kleisli composition
    let inline (>=>) f g = fun x -> f x >>= g
    /// Right-to-left Kleisli composition
    let inline (<=<) x = flip (>=>) x

    let foldM f s = 
        Seq.fold (fun acc t -> acc >>= (flip f) t) (returnM s)

//    let inline sequence s =
//        let inline cons a b = lift2 List.cons a b
//        List.foldBack cons s (returnM [])
//
//    let inline mapM f x = sequence (List.map f x)