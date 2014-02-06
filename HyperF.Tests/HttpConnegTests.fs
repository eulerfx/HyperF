namespace HyperF

open Http

type MediaTypeFormatter = string list * (HttpReq -> obj option)

module HttpContentTypeDecoders =

    //let rec fix f x = f (fix f) x
    let rec fix f = fun x -> f (fix f) x

    //let asyncParZipWith (f:'a -> 'b -> 'c) (l1:seq<'a>) (l2:seq<'b>) :  

    let test() = ()