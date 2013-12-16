module HyperF.Tests

open System
open Http
open Routing
open EkonBenefits.FSharp.Dynamic

module HttpRes = HttpResponses

[<EntryPoint>]
let main argv =    

//    let service = 
//        Filters.identity 
//        |> Filters.andThen Filters.printfnF 
//        |> Services.andThen HttpServices.helloWorld

    let routes = [
            
        Http.get "/" (fun (_,ri) -> HttpRes.plainText (sprintf "GET %s" ri.values?query))
            
        "/get_v2" =>> fun _ -> HttpRes.plainText "GETv2"

        "/get_file" =>> fun _ -> HttpRes.file @"C:\Users\eulerfx\Documents\GitHub\DDDInventoryItemFSharp\.gitignore"

        "/get_bind" =>> Routing.decodeModel (fun (_,_,x:int) -> HttpRes.plainText "hello")

        "/get_bind_2" =>> (^) (fun (_,_,model:int) -> HttpRes.plainText (sprintf "hello %A" model))

        "/get_bind_3" ^=>> fun (_,_,x:int) -> HttpRes.plainText "hello" 

        Get("/get_v3/:id") => fun _ -> HttpRes.plainText "GETv3"

        Post("/get_v3/:id") => 
            fun _ -> async {
                do! Async.Sleep(1000)
                return! HttpRes.plainText "GETv3" }

        Http.all (fun _ -> HttpRes.plainText "ALL")

    ] 
    
    //let routes = routes |> List.map (Routing.prefix "")

    let routeService = routes |> Routing.toService

    let service = 
        Filters.identity 
        |> Filters.andThen Filters.printfnF 
        |> Services.andThen routeService    

    Http.host "http://localhost:8081/" service |> Async.RunSynchronously
    
    //let nullProxy = Http.host (Http.client (Uri("http://www.google.com")))

    
    0
