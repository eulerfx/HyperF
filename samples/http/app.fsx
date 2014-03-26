#load "HyperF.fsx"

open HyperF
open Route

let service = 
    [
                 
    Get("/resource/:id") => fun (req,ri) -> "hello world!" |> HttpRes.plainText

    Put("/resource/:id") => fun (req,ri) -> async { 
        do! Async.Sleep(1000) 
        return! HttpRes.plainText "done!" }

    ] |> Route.toService

let filteredService = 
    Filter.identity 
    |> Filter.andThen Filter.printBeforeAfterTag
    |> Filter.toService service

Http.host "http://+:8081/" filteredService |> Async.RunSynchronously