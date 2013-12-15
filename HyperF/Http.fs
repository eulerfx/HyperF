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


module HttpFilters =

    let response (filter:HttpResponseFilter) (service:HttpService) : HttpService = 
        fun req -> async { let! res = service req in return! filter res service }
                     


module HttpResponses =
    
    let lift (response:HttpResponse) = Async.unit response

    let echo (request:HttpRequest) = {
        headers = request.headers
        body = request.body |> IO.echo }

    module PlainText =
        open System.Text

        let ofStr (str:string) = 
            { headers = Map.empty
              body = new MemoryStream(Encoding.UTF8.GetBytes(str)) } |> lift           


module HttpRequests =
    
    open System.Text

    let toString (request:HttpRequest) = 
        use sr = new StreamReader(request.body)
        sr.ReadToEndAsync()





module HttpServices =    

    let helloWorld (req:HttpRequest) = HttpResponses.PlainText.ofStr "hello world"

    let echo = HttpResponses.echo >> Async.unit


  


module Routing =

    type Route = HttpRequest * RouteInfo -> Async<HttpResponse> option

    and RouteInfo = { path : string }

    and Matcher = HttpRequest * RouteInfo -> bool


    module RouteInfos =
    
        let get (request:HttpRequest) = { path = request.url.AbsolutePath }   


    let route (matcher:Matcher) (service:HttpService) (request,routeInfo) = 
        if matcher (request,routeInfo) then service request |> Some
        else None                


    let toService (routes:seq<Route>) (req:HttpRequest) = 
        let routeInfo = RouteInfos.get req
        routes |> Seq.pick (fun route -> route (req,routeInfo))


    module Matches =        

        let (&&&) (m1:Matcher) (m2:Matcher) = fun a -> (m1 a && m2 a)

        //let appendOr (m1:Matcher) (m2:Matcher) = fun a -> (m1 a || m2 a)

        let pathEq (path:string) (request:HttpRequest,routeInfo:RouteInfo) = Strings.equalToIgnoreCase path request.url.AbsolutePath

        let httpMethod (httpMethod:string) (request:HttpRequest,routeInfo:RouteInfo) = Strings.equalToIgnoreCase httpMethod request.httpMethod

        let ALL _ = true

        let get path = (httpMethod "GET") &&& (pathEq path)        



module Http =

    let get path = Routing.route (Routing.Matches.get path)

    let all = Routing.route (Routing.Matches.ALL)
             

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
            


    let host (service:HttpService) = async {

        let listener = new HttpListener()
        listener.Prefixes.Add("http://localhost:8081/")
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








