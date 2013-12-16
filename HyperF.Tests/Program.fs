module HyperF.Tests

open System
open Http
open EkonBenefits.FSharp.Dynamic

[<EntryPoint>]
let main argv =    

    let service = 
        Filters.identity 
        |> Filters.andThen Filters.printfnF 
        |> Services.andThen HttpServices.helloWorld
    

    let routes = [
            
        Http.get "/" (fun _ ri -> HttpResponses.plainText (sprintf "GET %s" ri.values?query))
            
        "/get_v2" =>> fun _ _ -> HttpResponses.plainText "GETv2"

        Get("/get_v3/:id") => fun _ _ -> HttpResponses.plainText "GETv3"

        Post("/get_v3/:id") => 
            fun _ _ -> async {
                do! Async.Sleep(1000)
                return! HttpResponses.plainText "GETv3" }

        Http.all (fun _ _ -> HttpResponses.plainText "ALL")

    ]

    let routeService = routes |> Routing.toService

    let service = 
        Filters.identity 
        |> Filters.andThen Filters.printfnF 
        |> Services.andThen routeService    

    Http.host "http://localhost:8081/" service |> Async.RunSynchronously
    
    //let nullProxy = Http.host (Http.client (Uri("http://www.google.com")))

    
    0
