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

and HttpRep = HttpResponse


type HttpService = Service<HttpRequest, HttpResponse>

type HttpFilter = Filter<HttpRequest, HttpResponse>

type HttpResponseFilter = HttpResponse -> Service<HttpRequest, HttpResponse> -> Async<HttpResponse>



type HttpAuthReq = HttpAuthReq of HttpReq * user:string




module HttpFilters =

    open Async

    let response (filter:HttpResponseFilter) (service:HttpService) : HttpService = 
        fun req -> async { let! res = service req in return! filter res service }
                     
    let authReq (req:HttpReq) = async { return HttpAuthReq(req,"foo@bar") }

    let auth req service = authReq req >>= service

    let context provider req service = (req,provider req) |> service

    let dynamicContext req service = context (fun _ -> new System.Dynamic.ExpandoObject() :> obj) req service


module HttpResponses =
    
    let private lift (response:HttpResponse) = Async.unit response

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




module HttpRequests =
    
    open System.Text

    let toString (request:HttpRequest) = 
        use sr = new StreamReader(request.body)
        sr.ReadToEndAsync()





module HttpServices =    

    let helloWorld _ = HttpResponses.plainText "hello world"

    let echo = HttpResponses.echo >> Async.unit


  


module Routing =

    open EkonBenefits.FSharp.Dynamic


    type Route = IsMatch * (HttpReq * RouteInfo -> Async<HttpResponse>)

    and RouteInfo = { 
        path   : string
        values : obj }

    and IsMatch = HttpReq * RouteInfo -> bool
        

    let decodeModel (service:HttpReq * RouteInfo * 'a -> Async<HttpResponse>) =
        fun (req,ri) ->
            let a = Unchecked.defaultof<'a> // TODO: decode via content headers, or decode via filter into value?
            service (req,ri,a)

        
    let (^) = decodeModel


    module RouteInfos =
    
        let prefix prefix (ri) = { ri with path = prefix + ri.path }

        let parse (req:HttpReq) = 

            let path = req.url.AbsolutePath
            
            let values = new System.Dynamic.ExpandoObject()
            values?path <- path
            values?query <- req.url.Query

            { path   = req.url.AbsolutePath
              values = values }   

    let private exec (req,ri) (route:Route) =
         let matches,service = route
         if matches (req,ri) then service (req,ri) |> Some
         else None

    let toService (routes:seq<Route>) =
        fun (req:HttpRequest) ->
            let ri = RouteInfos.parse req
            routes |> Seq.pick (exec (req,ri))


    module Match =        

        let ALL _ = true

        let And (m1:IsMatch) (m2:IsMatch) = fun a -> (m1 a && m2 a)

        let (&&&) = And

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
        (Match.prefix prefix isMatch),service


module Http =

    open Routing

    let inline private methodPath methodMatch path = tuple (methodMatch |> Match.And <| Match.pathExact path)


    let get path = methodPath Routing.Match.get path    
    /// HTTP GET
    let (=>>) = get

    let (^=>>) path service = methodPath Routing.Match.get path (service |> Routing.decodeModel)


    let put path = methodPath Match.put path
    let (==>) = put

    let delete path = methodPath Match.delete path
    let (-=>) = delete

    let post path = methodPath Match.post path
    let (!=>) = post


    type HttpMethodRoute = Get of path:string | Put of string | Post of string | Delete of string

    let (=>) httpMethodRoute = 
        match httpMethodRoute with
        | Get path -> methodPath Match.get path
        | Put path -> methodPath Match.put path
        | Post path -> methodPath Match.post path
        | Delete path -> methodPath Match.delete path
        


    let all service = (Routing.Match.ALL,service)
             



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


    let host uriPrefix (service:HttpService) = async {

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








