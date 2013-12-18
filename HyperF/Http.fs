namespace HyperF

open System
open System.IO
open System.Text



type HttpRequest = {    
    url : Uri
    httpMethod : string
    headers : Map<string, string list>
    body : Stream
}

and HttpReq = HttpRequest


type HttpResponse = {
    headers : Map<string, string list>
    body : Stream
}

and HttpResp = HttpResponse



type HttpAuthReq = HttpAuthReq of HttpReq * user:string



module HttpRes =
    
    let private lift (response:HttpResp) = Async.unit response

    let echo (request:HttpRequest) = {
        headers = request.headers
        body = request.body |> IO.echo }

    let fromContentTypeAndStream (contentType:string) (stream:Stream) =
        { headers = Map.empty
          body = stream } |> lift

    let fromContentTypeAndBytes (contentType:string) (bytes:byte array) = fromContentTypeAndStream contentType (new MemoryStream(bytes))
    
    let plainText (str:string) = fromContentTypeAndBytes "text/plain" (Encoding.UTF8.GetBytes(str))

    let file path = fromContentTypeAndStream "text/plain" (File.OpenRead(path))

    open Newtonsoft.Json
    
    let json (obj:obj) =        
        let json = JsonConvert.SerializeObject(obj) in
        fromContentTypeAndBytes "application/json" (Encoding.UTF8.GetBytes(json))


    module Jade =
        
        let view viewName viewData = 0







module HttpService =    

    let helloWorld _ = HttpRes.plainText "hello world"

    let echo = HttpRes.echo >> Async.unit


  
module HttpFilter =

    open Async
                     
    let authReq (req:HttpReq) = async { return HttpAuthReq(req,"foo@bar") }

    let auth req service = authReq req >>= service

    let context provider = Filter.fromMapReqSync (fun req -> req,provider req)

    module Json =

        open Newtonsoft.Json

        let private jsonSer = new JsonSerializer()

        let private decodeDynamic (req:HttpReq) = 
            let sr = new StreamReader(req.body)
            jsonSer.Deserialize(sr, typeof<System.Dynamic.DynamicObject>)

        let private encode obj =             
            let s = new MemoryStream()
            let sw = new StreamWriter(s)
            jsonSer.Serialize(sw,obj)
            HttpRes.fromContentTypeAndStream "application/json" s

        let dynamic = 
            Filter.fromMap 
                (fun (req:HttpReq) -> (req,(decodeDynamic req)) |> Async.unit) 
                (fun (res:obj) -> res |> HttpRes.json)


module Route =

    open EkonBenefits.FSharp.Dynamic


    type Route = HttpReq * RouteInfo -> Async<HttpResp> option

    and RouteInfo = { 
        path   : string
        values : obj }

    and IsMatch = HttpReq * RouteInfo -> bool


    module RouteInfos =
    
        let prefix prefix (ri) = { ri with path = prefix + ri.path }

        let parse (req:HttpReq) = 

            let path = req.url.AbsolutePath
            
            let values = new System.Dynamic.ExpandoObject()
            values?path <- path
            values?query <- req.url.Query

            { path   = req.url.AbsolutePath
              values = values }   


    let identity : Route = fun (req,ri) -> None

    let append (r1:Route) (r2:Route) = fun req -> [r1;r2] |> Seq.tryPick (fun route -> route req)

    let ofMatch (m:IsMatch) service = 
        fun (req,ri) ->
            if m (req,ri) then service (req,ri) |> Some
            else None

    let model (service:HttpReq * RouteInfo * 'a -> Async<HttpResponse>) =
        fun (req,ri) ->
            let a = Unchecked.defaultof<'a> // TODO: decode via content headers, or decode via filter into value?
            service (req,ri,a)
        
    let (^) = model

    let toService (routes:seq<Route>) =
        fun (req:HttpRequest) ->            
            let route = routes |> Seq.fold append identity
            let ri = RouteInfos.parse req
            match route (req,ri) with
            | Some res -> res
            | None _ -> failwith "No matching route!"


    module Match =        

        let ALL _ = true

        let And (m1:IsMatch) (m2:IsMatch) = fun a -> (m1 a && m2 a)

        let (&&&) = And

        let Or (m1:IsMatch) (m2:IsMatch) = fun a -> (m1 a || m2 a)

        let pathExact (path:string) (req:HttpRequest,ri:RouteInfo) = Strings.equalToIgnoreCase path ri.path                

        let prefix (prefix:string) (m:IsMatch) : IsMatch = 
            fun (req,ri) -> 
                let ri = ri |> RouteInfos.prefix prefix
                m (req,ri) 

        let httpMethod (httpMethod:string) (req:HttpRequest,ri:RouteInfo) = Strings.equalToIgnoreCase httpMethod req.httpMethod

        let get = httpMethod "GET"
        
        let put = httpMethod "PUT"

        let delete = httpMethod "DELETE"

        let post = httpMethod "POST"


    let prefix prefix route = 
        let isMatch,service = route in
        ofMatch (Match.prefix prefix isMatch) service


module Http =

    let inline private methodPath methodMatch path = Route.ofMatch (methodMatch |> Route.Match.And <| Route.Match.pathExact path)

    type RouteMatchCode = Get of path:string | Put of string | Post of string | Delete of string | All

    let (=>) = function
        | Get path    -> methodPath Route.Match.get path
        | Put path    -> methodPath Route.Match.put path
        | Post path   -> methodPath Route.Match.post path
        | Delete path -> methodPath Route.Match.delete path
        | All         -> Route.ofMatch (Route.Match.ALL)       


    open System.Net
    open System.Net.Http
    open System.Threading

    let ofClient (httpClient:HttpClient) =

        fun (req:HttpReq) -> async {

            let reqMsg = new HttpRequestMessage()
            reqMsg.RequestUri <- req.url
            reqMsg.Method <- 
                match req.httpMethod with
                | "POST" -> HttpMethod.Post
                | "GET" -> HttpMethod.Get
                | "PUT" -> HttpMethod.Put
                | "DELETE" -> HttpMethod.Delete
                | _ -> HttpMethod.Get

            reqMsg.Content <- new StreamContent(req.body)                          

            let! repMsg = httpClient.SendAsync(reqMsg) |> Async.AwaitTask
            let! repBody = repMsg.Content.ReadAsStreamAsync() |> Async.AwaitTask

            return {
                headers = Map.empty
                body = repBody } 
            }
    
    let client baseUrl = 
        let httpClient = new HttpClient()
        httpClient.BaseAddress <- baseUrl
        ofClient httpClient        


    let host uriPrefix (service:Service<HttpReq, HttpResp>) = async {

        let listener = new HttpListener()
        listener.Prefixes.Add(uriPrefix)
        listener.AuthenticationSchemes <- AuthenticationSchemes.Anonymous
        listener.Start()

        let proc (ctx:HttpListenerContext) = async {

            let request = { 
                url = ctx.Request.Url
                httpMethod = ctx.Request.HttpMethod
                headers = Map.empty
                body = ctx.Request.InputStream }

            let! response = service request
            do! response.body.CopyToAsync(ctx.Response.OutputStream) |> Async.AwaitIAsyncResult |> Async.Ignore
            ctx.Response.OutputStream.Dispose()
            ctx.Response.Close() }

        let ct = CancellationToken.None

        let worker = async {
            while not <| ct.IsCancellationRequested do
                let! ctx = listener.GetContextAsync() |> Async.AwaitTask
                do! proc ctx }

        printfn "Starting server..."

        return! worker
        //Async.Start (worker, ct)

        }








