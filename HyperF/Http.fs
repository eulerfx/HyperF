namespace HyperF

open System
open System.IO



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


    open Newtonsoft.Json
    open System.Text


    let json (obj:obj) =        
        let json = JsonConvert.SerializeObject(obj) in
        fromContentTypeAndBytes "application/json" (Encoding.UTF8.GetBytes(json))


    let plainText (str:string) = fromContentTypeAndBytes "text/plain" (Encoding.UTF8.GetBytes(str))


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

    open System.Dynamic
    open EkonBenefits.FSharp.Dynamic

    type Route = HttpRequest * RouteInfo -> Async<HttpResponse> option

    and RouteInfo = { 
        path   : string
        values : obj }

    and Match = HttpRequest * RouteInfo -> bool


    module RouteInfos =
    
        let get (request:HttpRequest) = 
            let path = request.url.AbsolutePath
            
            let values = new ExpandoObject() :> obj
            values?path <- path
            values?query <- request.url.Query

            { path   = request.url.AbsolutePath
              values = values }   


    let route (matcher:Match) (service:HttpReq -> RouteInfo -> Async<HttpResponse>) =
        fun (request,routeInfo) ->
            if matcher (request,routeInfo) then service request routeInfo |> Some
            else None                


    let toService (routes:seq<Route>) (req:HttpRequest) = 
        let routeInfo = RouteInfos.get req
        routes |> Seq.pick (fun route -> route (req,routeInfo))


    module Matches =        

        let And (m1:Match) (m2:Match) = fun a -> (m1 a && m2 a)

        let (&&&) (m1:Match) (m2:Match) = fun a -> (m1 a && m2 a)

        //let appendOr (m1:Matcher) (m2:Matcher) = fun a -> (m1 a || m2 a)

        let pathEq (path:string) (request:HttpRequest,routeInfo:RouteInfo) = Strings.equalToIgnoreCase path request.url.AbsolutePath        

        let ALL _ = true

        let httpMethod (httpMethod:string) (request:HttpRequest,routeInfo:RouteInfo) = Strings.equalToIgnoreCase httpMethod request.httpMethod

        let get = (httpMethod "GET") //&&& (pathEq path)
        
        let put = httpMethod "PUT"

        let delete = httpMethod "DELETE"

        let post = httpMethod "POST"



module Http =

    open Routing

    let inline private methodPath methodMatch path = Routing.route (methodMatch |> Matches.And <| Matches.pathEq path)


    let get path = methodPath Routing.Matches.get path
    
    /// HTTP GET
    let (=>>) = get

    let put path = methodPath Matches.put path
    let (==>) = put

    let delete path = methodPath Matches.delete path
    let (-=>) = delete

    let post path = methodPath Matches.post path
    let (!=>) = post


    type HttpMethodRoute = Get of path:string | Put of string | Post of string | Delete of string

    let (=>) httpMethodRoute = 
        match httpMethodRoute with
        | Get path -> methodPath Matches.get path
        | Put path -> methodPath Matches.put path
        | Post path -> methodPath Matches.post path
        | Delete path -> methodPath Matches.delete path
        


    let all service = Routing.route (Routing.Matches.ALL) service
             



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
            do! response.body.CopyToAsync(ctx.Response.OutputStream) |> Async.AwaitIAsyncResult |> Async.Ignore }

        let ct = CancellationToken.None

        let worker = async {
            while not <| ct.IsCancellationRequested do
                let! ctx = listener.GetContextAsync() |> Async.AwaitTask
                do! proc ctx
                ctx.Response.Close() 
            }

        printfn "Starting server..."

        return! worker
        //Async.Start (worker, ct)

        }








