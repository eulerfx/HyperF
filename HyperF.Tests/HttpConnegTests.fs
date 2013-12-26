namespace HyperF

open Http

type MediaTypeFormatter = string list * (HttpReq -> obj option)


module HttpContentTypeDecoders =

    let test() = ()