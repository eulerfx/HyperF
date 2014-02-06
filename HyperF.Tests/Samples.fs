namespace HyperF

open Http
open Route

module BasicSample = 

    let run() =

        let routes = [
            
            Get("/get_v3/:id") => fun _ -> HttpRes.plainText "GETv3"

            Get("/get_v3_bind/:id") => (^) (fun (req,ri,model:int) -> HttpRes.plainText (sprintf "GETv3 bind %i" model))

            Get("/get_v3_bind_v2/:id") => Route.model (fun (req,ri,model:int) -> HttpRes.plainText (sprintf "GETv3 bind %i" model))

            Post("/get_v3/:id") => 
                fun _ -> async {
                    do! Async.Sleep(1000)
                    return! HttpRes.plainText "GETv3" }

            Sub("/sub") ==> [
            
                Get("/nested_1") => fun _ -> HttpRes.plainText "/sub/nested_1"

            ]    

            All => (fun _ -> HttpRes.plainText "ALL")
        ] 
    
        //let nested = routes |> Route.nest (Sub("/parent"))

        let routeService = routes |> Route.toService

        let service = 
            Filter.identity 
            |> Filter.andThen Filter.printfnF
            |> Filter.toService routeService

        Http.host "http://+:8081/" service |> Async.RunSynchronously


module AsyncStreamSample = 

    open System
    open System.Text
    open FSharp.Control
    open EkonBenefits.FSharp.Dynamic

    let run() =
           
        let rows n s = 
            Seq.init n (fun i -> sprintf "this is %s in row %d" s i)
            |> Seq.map (fun row -> Encoding.UTF8.GetBytes(row + Environment.NewLine))
            |> AsyncSeq.ofSeq                

        let routes = [
            
            Get("/data/:user/?c={c}") => 
                fun (req,ri) ->
                    rows (ri.values?c |> Int32.Parse) (ri.values?user)
                    |> AsyncSeqStream.ofByteRows 
                    |> HttpRes.fromMediaTypeAndStream "text/plain"
        ]


        let service = routes |> Route.toService

        Http.host "http://+:8081/" service |> Async.RunSynchronously


module DbAccessSample = 

    open System
    open System.Text
    open System.Data
    open System.Data.SqlClient

    open FSharp.Control

    let readDataRecords (cmd:SqlCommand) = asyncSeq {
        let! reader = cmd.ExecuteReaderAsync() |> Async.AwaitTask
        let! read = reader.ReadAsync() |> Async.AwaitTask
        yield reader :> IDataRecord }


    type Reader<'c, 'a> = 'c -> 'a

    module Reader =
        
        let returnR a : Reader<'c, 'a> = fun _ -> a

        let map (m:Reader<'c, 'a>) (f:'a -> 'b) : Reader<'c, 'b> = fun c -> f (m c)

        let bind (m:Reader<'c, 'a>) (f:'a -> Reader<'c, 'b>) : Reader<'c, 'b> = fun c -> (f (m c)) c


    let run() =
    
        let connStr = ""        

        let provideConn connStr =
            let conn = new SqlConnection(connStr)                        
            conn.Open()
            fun (m:Reader<SqlConnection, 'r>) ->
                try m conn
                finally conn.Close()


        let getData (conn:SqlConnection) (id:string) =
            let cmd = conn.CreateCommand()
            cmd.CommandText <- "SELECT 1 as One"
            cmd.Parameters.AddWithValue("@id", id) |> ignore
            readDataRecords cmd |> AsyncSeq.map (fun r -> "")

        
        let provideConn = provideConn connStr
        let getData = provideConn getData
        let getData id = getData id |> AsyncSeq.map (fun row -> Encoding.UTF8.GetBytes(row + Environment.NewLine))

        let routes = [
            
            Get("/data/:id") => fun (req,ri) -> HttpRes.plainText "DATA!"

        ]


        let service = routes |> Route.toService



        ()
