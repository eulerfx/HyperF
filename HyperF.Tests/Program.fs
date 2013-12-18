module HyperF.Tests

open System
open Http
open Route
open EkonBenefits.FSharp.Dynamic

[<EntryPoint>]
let main argv =    

    let routes = [
            
        Get("/get_v3/:id") => fun _ -> HttpRes.plainText "GETv3"

        Get("/get_v3_bind/:id") => (^) (fun (req,ri,model:int) -> HttpRes.plainText (sprintf "GETv3 bind %i" model))

        Get("/get_v3_bind_v2/:id") => Route.model (fun (req,ri,model:int) -> HttpRes.plainText (sprintf "GETv3 bind %i" model))

        Post("/get_v3/:id") => 
            fun _ -> async {
                do! Async.Sleep(1000)
                return! HttpRes.plainText "GETv3" }

        All => (fun _ -> HttpRes.plainText "ALL")
    ]       
         
    let routeService = routes |> Route.toService

    let service = 
        Filter.identity 
        |> Filter.combine Filter.printfnF 
        |> Filter.toService routeService

    Http.host "http://localhost:8081/" service |> Async.RunSynchronously
    
    //let nullProxy = Http.host (Http.client (Uri("http://www.google.com")))
    
    0
