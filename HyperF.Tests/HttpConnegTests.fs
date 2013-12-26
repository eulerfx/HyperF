namespace HyperF

open Http

type MediaTypeFormatter = string list * (HttpReq -> obj option)


module HttpContentTypeDecoders =

    module Json =

        open System.IO
        open Newtonsoft.Json

        [<Literal>]
        let ContentType = "application/json"

        let private jsonSer = new JsonSerializer()

        let private decodeDynamic (s:Stream) = 
            let sr = new StreamReader(s)
            jsonSer.Deserialize(sr, null)

        let private encode obj =             
            let ms = new MemoryStream()
            let sw = new StreamWriter(ms)
            jsonSer.Serialize(sw, obj)
            HttpRes.fromContentTypeAndStream ContentType ms

        let dynamic = 
            Filter.fromMap 
                (fun (req:HttpReq) -> (req,decodeDynamic req.body) |> Async.unit) 
                (fun (res:obj) -> res |> HttpRes.json)