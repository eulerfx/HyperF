namespace HyperF

module Cont = 

    type Cont<'a,'r> = ('a  -> 'r) -> 'r

    let bind (m:Cont<'a, 'r>) k c = m (fun a -> k a c)


module Strings =

    open System

    let inline equalTo (comp:StringComparison) (a:string) (b:string) = String.Compare(a, b, comp) = 0

    let equalToIgnoreCase = equalTo (StringComparison.OrdinalIgnoreCase)
