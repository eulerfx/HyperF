namespace HyperF

open System
open System.IO
open System.Text
open System.Net
open System.Net.Http
open System.Net.Http.Headers


type HttpReq = HttpRequestMessage

type HttpResp = HttpResponseMessage

type HttpAuthReq = HttpAuthReq of HttpReq * user:string


module HttpRes =   

    let echo (req:HttpReq) = async {
        let! stream = req.Content.ReadAsStreamAsync() |> Async.AwaitTask
        let res = new HttpResponseMessage(HttpStatusCode.OK)
        res.RequestMessage <- req
        res.Content <- new StreamContent(stream |> IO.echo)
        
        return res }

    let fromMediaTypeAndStream (mediaType:string) (stream:Stream) = async {
        let res = new HttpResponseMessage(HttpStatusCode.OK)
        let content = new StreamContent(stream)
        content.Headers.ContentType <- new MediaTypeHeaderValue(mediaType)
        res.Content <- content
        return res }

    let fromMediaTypeAndBytes (mediaType:string) (bytes:byte array) = fromMediaTypeAndStream mediaType (new MemoryStream(bytes))
    
    let plainText (str:string) = fromMediaTypeAndBytes "text/plain" (Encoding.UTF8.GetBytes(str))

    // TODO: refactor, somehow, to use Socket.SendFile. for example, declare SendFile request as a value to be handled by hosting infrastructure.
    let file path = fromMediaTypeAndStream "text/plain" (File.OpenRead(path))

    let statusCode (statusCode) = new HttpResponseMessage(statusCode) |> Async.returnA

    
    module StatusCode =

        let notFound404() = statusCode HttpStatusCode.NotFound
        


    open Newtonsoft.Json
    
    let json (obj:obj) =        
        let json = JsonConvert.SerializeObject(obj) in
        fromMediaTypeAndBytes "application/json" (Encoding.UTF8.GetBytes(json))


    module Jade =
        
        let view viewName viewData = 0







module HttpService =    

    let helloWorld _ = HttpRes.plainText "hello world"

    let echo = HttpRes.echo // >> Async.unit


  
module HttpFilter =

    open Async
                     
    let authReq (req:HttpReq) = async { return HttpAuthReq(req,"foo@bar") }

    let auth req service = authReq req >>= service

    let context provider = Filter.fromMapReqSync (fun req -> req,provider req)

    module Json =

        open Newtonsoft.Json

        [<Literal>]
        let MediaType = "application/json"

        let private jsonSer = new JsonSerializer()

        let private decodeDynamic (req:HttpReq) = async {
            let! stream = req.Content.ReadAsStreamAsync() |> Async.AwaitTask
            let sr = new StreamReader(stream)
            return jsonSer.Deserialize(sr, null) }

        let private encode obj =             
            let s = new MemoryStream()
            let sw = new StreamWriter(s)
            jsonSer.Serialize(sw,obj)
            HttpRes.fromMediaTypeAndStream MediaType

        let dynamic = 
            Filter.fromMap 
                (fun (req:HttpReq) -> async { let! decoded = decodeDynamic req in return (req,decoded) }) 
                (fun (res:obj) -> res |> HttpRes.json)



module Http =    

    open System.Net
    open System.Net.Http
    open System.Threading

    let ofClient (httpClient:HttpClient) = fun (req:HttpReq) -> httpClient.SendAsync(req) |> Async.AwaitTask
    
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

            let httpMethod = 
                match ctx.Request.HttpMethod.ToUpperInvariant() with
                | "GET"     -> HttpMethod.Get
                | "POST"    -> HttpMethod.Post
                | "PUT"     -> HttpMethod.Put
                | "DELETE"  -> HttpMethod.Delete
                | "HEAD"    -> HttpMethod.Head
                | "OPTIONS" -> HttpMethod.Options
                | "TRACE"   -> HttpMethod.Trace
                | m         -> new HttpMethod(m)


            use request = new HttpRequestMessage(httpMethod, ctx.Request.Url)
            request.Headers.Referrer <- ctx.Request.UrlReferrer
            
            for i in [0..ctx.Request.Headers.Count - 1] do
                let name = ctx.Request.Headers.GetKey(i)
                let values = ctx.Request.Headers.GetValues(i)
                request.Headers.Add(name, values)

            request.Content <- new StreamContent(ctx.Request.InputStream)

            use! response = service request

            response.Headers
            |> Seq.iter (fun pair ->
                pair.Value
                |> Seq.iter (fun value -> ctx.Response.Headers.Add(pair.Key,value))                  
            )

            ctx.Response.ProtocolVersion <- response.Version

            do! response.Content.CopyToAsync(ctx.Response.OutputStream) |> Async.AwaitIAsyncResult |> Async.Ignore
            
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








