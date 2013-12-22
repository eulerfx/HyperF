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


    let statusCode (httpStatusCode:int) = 
        { headers = Map.empty
          body = null } |> lift

    
    module StatusCode =

        let notFound404() = statusCode 404
        


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



module Http =    

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








