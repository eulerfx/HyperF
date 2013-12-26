namespace HyperF

module Route =

    open System
    open System.Net
    open System.Net.Http

    open EkonBenefits.FSharp.Dynamic


    type Route = HttpReq * RouteInfo -> Async<HttpResp> option

    and RouteInfo = { values : obj }        
        with 

            static member parse (req:HttpReq) = { RouteInfo.values = new System.Dynamic.ExpandoObject() } 

            member ri.getValue(name) = Dynamitey.Dynamic.InvokeGet(ri.values, name)

            member ri.setValue(name,value) = Dynamitey.Dynamic.InvokeSet(ri.values, name, value) |> ignore

            member ri.mergeMap (values:Map<string,string>) =                 
                values |> Map.iter (fun k v -> ri.setValue(k, v))
                ri

    and IsMatch = HttpReq * RouteInfo -> RouteInfo option



    let identity : Route = fun (req,ri) -> None

    let append (r1:Route) (r2:Route) = fun req -> [r1;r2] |> Seq.tryPick (fun route -> route req)

    let fromMatch (m:IsMatch) service = 
        fun (req,ri) -> 
            match m (req,ri) with
            | Some ri -> service (req,ri) |> Some
            | None -> None

    /// adapts a model based serivce				
    let model (service:HttpReq * RouteInfo * 'a -> Async<HttpResp>) =
        fun (req,ri) ->
            let a = Unchecked.defaultof<'a> // TODO: decode via content headers, or decode via filter into value?
            service (req,ri,a)
        
    let (^) = model


    module Match =        

        let inline private konst pred (req,ri) = if pred (req,ri) then Some ri else None

        let inline withRi ri flag = if flag then Some(ri) else None

        let ALL = konst (fun _ -> true)

        let And (m1:IsMatch) (m2:IsMatch) = 
            fun (req,ri) -> 
                m1 (req,ri) |> Option.bind (fun ri -> m2 (req,ri))

        let pathExact (path:string) (req:HttpReq,ri:RouteInfo) = Strings.equalToIgnoreCase path req.RequestUri.AbsolutePath |> withRi ri

        let pathPrefix (pathPrefix:string) (req:HttpReq,ri:RouteInfo) = Strings.startsWithIngoreCase pathPrefix req.RequestUri.AbsolutePath |> withRi ri
        
        let pattern (pattern:string) = 
            let pat = RoutePattern.matchUrl pattern
            fun (req:HttpReq,ri:RouteInfo) -> 
                pat req.RequestUri |> Option.map (fun values -> ri.mergeMap(values))

        let httpMethod (httpMethod:HttpMethod) (req:HttpReq,ri:RouteInfo) = httpMethod = req.Method |> withRi ri

//        let get = httpMethod HttpMethod.Get
//        
//        let put = httpMethod HttpMethod.Put
//
//        let delete = httpMethod HttpMethod.Delete
//
//        let post = httpMethod HttpMethod.Post

        let inline methodAndPattern httpMeth pat = ((httpMethod httpMeth) |> And <| pattern pat)



    type RouteMatchPattern = 
        | Get of pattern:string | Put of pattern:string | Post of pattern:string | Delete of pattern:string 
        | Sub of pattern:string
        | All
    
    let inline private patternToMatch pat = 
        match pat with
        | Get path    -> Match.methodAndPattern HttpMethod.Get path
        | Put path    -> Match.methodAndPattern HttpMethod.Put path
        | Post path   -> Match.methodAndPattern HttpMethod.Post path
        | Delete path -> Match.methodAndPattern HttpMethod.Delete path
        | Sub path    -> Match.pathPrefix path
        | All         -> Match.ALL
    
    let inline private patternToRoute (pat,service) = 
        let isMatch = patternToMatch pat
        fromMatch isMatch service

    /// converts a set of routes into a serivce given an escape hatch result.
    let private toServiceCont cont routes =        
        let route = routes |> Seq.map patternToRoute |> Seq.fold append identity
        fun (req:HttpReq) ->
            let ri = RouteInfo.parse req
            match route (req,ri) with
            | Some result -> result
            | None -> cont

    let toService routes = routes |> List.concat |> toServiceCont (HttpRes.StatusCode.notFound404())

    let (=>) (pat:RouteMatchPattern) (service:Service<_,_>) = [(pat,service)]

    let private nestPat = function
        
        | Sub p, Get pp    -> Get(p + pp)
        | Sub p, Put pp    -> Put(p + pp)
        | Sub p, Post pp   -> Post(p + pp)
        | Sub p, Delete pp -> Delete(p + pp)

        | All, Get pp      -> Get(pp)

        | _ -> failwith "Invalid nesting!"

    let private nestInner (parentPat:RouteMatchPattern) (routes:(RouteMatchPattern * 'a) list list) =
        routes 
        |> List.concat 
        |> List.map (fun (pat,service) -> nestPat(parentPat,pat),service)
                
    let (==>) pat routes = nestInner pat routes

    let nest parentPat routes = [ nestInner parentPat routes ]