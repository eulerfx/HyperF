module HyperF.Tests

[<EntryPoint>]
let main argv =    

    let service = 
        Filters.identity 
        |> Filters.andThen Filters.printfnF 
        |> Services.andThen HttpServices.helloWorld
    
    let routes = [
        Http.get "/" (fun req -> HttpResponses.PlainText.ofStr "hello")
        Http.all (fun req -> HttpResponses.PlainText.ofStr "ALL")
    ]

    let routeService = routes |> Routing.toService

    let service = 
        Filters.identity 
        |> Filters.andThen Filters.printfnF 
        |> Services.andThen routeService    

    Http.host service |> Async.RunSynchronously
    0
