namespace HyperF

module Route =

    open EkonBenefits.FSharp.Dynamic


    type Route = HttpReq * RouteInfo -> Async<HttpResp> option

    and RouteInfo = { 
        path   : string
        values : obj }

    and IsMatch = HttpReq * RouteInfo -> bool


    module RouteInfos =
    
        let parse (req:HttpReq) = 

            let path = req.url.AbsolutePath
            
            let values = new System.Dynamic.ExpandoObject()
            values?path <- path
            values?query <- req.url.Query

            { path   = req.url.AbsolutePath
              values = values }   


    let identity : Route = fun (req,ri) -> None

    let append (r1:Route) (r2:Route) = fun req -> [r1;r2] |> Seq.tryPick (fun route -> route req)

    let fromMatch (m:IsMatch) service = 
        fun (req,ri) ->
            if m (req,ri) then service (req,ri) |> Some
            else None

    /// adapts a model based serivce				
    let model (service:HttpReq * RouteInfo * 'a -> Async<HttpResponse>) =
        fun (req,ri) ->
            let a = Unchecked.defaultof<'a> // TODO: decode via content headers, or decode via filter into value?
            service (req,ri,a)
        
    let (^) = model

    module Match =        

        let ALL _ = true

        let And (m1:IsMatch) (m2:IsMatch) = fun a -> (m1 a && m2 a)

        let (&&&) = And

        let Or (m1:IsMatch) (m2:IsMatch) = fun a -> (m1 a || m2 a)

        let pathExact (path:string) (req:HttpRequest,ri:RouteInfo) = Strings.equalToIgnoreCase path ri.path

        let pathPrefix (pathPrefix:string) (req:HttpRequest,ri:RouteInfo) = Strings.startsWithIngoreCase pathPrefix ri.path                

        let httpMethod (httpMethod:string) (req:HttpRequest,ri:RouteInfo) = Strings.equalToIgnoreCase httpMethod req.httpMethod

        let get = httpMethod "GET"
        
        let put = httpMethod "PUT"

        let delete = httpMethod "DELETE"

        let post = httpMethod "POST"

        let methodAndPattern httpMeth pattern = ((httpMethod httpMeth) |> And <| pathExact pattern)


    type RouteMatchPattern = 
        | Get of pattern:string | Put of pattern:string | Post of pattern:string | Delete of pattern:string 
        | Sub of pattern:string
        | All
    
    let inline private patternToMatch pat = 
        match pat with
        | Get path    -> Match.methodAndPattern "GET" path
        | Put path    -> Match.methodAndPattern "PUT" path
        | Post path   -> Match.methodAndPattern "POST" path
        | Delete path -> Match.methodAndPattern "DELETE" path
        | Sub path    -> Match.pathPrefix path
        | All         -> Match.ALL
    
    let inline private patternToRoute (pat,service) = 
        let isMatch = patternToMatch pat
        fromMatch isMatch service

    let private toServiceFull cont routes =        
        let route = routes |> Seq.map patternToRoute |> Seq.fold append identity
        fun (req:HttpReq) ->
            let ri = RouteInfos.parse req
            match route (req,ri) with
            | Some result -> result
            | None -> cont

    let toService routes = routes |> List.concat |> toServiceFull (HttpRes.StatusCode.notFound404())

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
                
    let (==>) code routes = nestInner code routes

    let nest parentPat routes = [ nestInner parentPat routes ]