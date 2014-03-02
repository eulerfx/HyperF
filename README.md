HyperF
======

F# async service framework inspired in part by [Your server as a function](http://monkey.org/~marius/funsrv.pdf). The framework provides combinators for implementing various service interaction patterns.

## Central Abstractions

### Service

```fsharp
type Service<'Req, 'Res> = 'Req -> Async<'Res>
```

A service is a function from a request onto an asynchronous response. A service and a service client are symmetric - expressed by the same interface.

### Filter

```fsharp
type Filter<'Req, 'ReqInner, 'ResInner, 'Res> = 'Req -> Service<'ReqInner, 'ResInner> -> Async<'Res>
```

Filters compose and they allow interception of service requests and responses. They can be used to implement various cross-cutting concerns, such as authentication, logging, metrics, serialization, etc. Filters can be interpretted as Kleisli arrows onto the continuation monad and therefore they compose via Kleisli composition.


## Modules

### HTTP Module

The HTTP module contains combinators for HTTP services carried by message types in ```System.Net.Http```.

```fsharp
#r "HyperF.dll"

open Http
open Route

let routes = [
     
    Get("/get_plain_text/:id") => fun _ -> HttpRes.plainText "hello world"

    Get("/echo_id/:id") => (^) (fun (req,ri,model:int) -> HttpRes.plainText (sprintf "id=%i" model))

    Post("/post/:id") => 
        fun _ -> async {
            do! Async.Sleep(1000)
            return! HttpRes.plainText "post" }

    Sub("/sub") ==> [
    
        Get("/nested") => fun _ -> HttpRes.plainText "/sub/nested"

    ]    

    All => (fun _ -> HttpRes.plainText "*")
] 

let httpService = 
    Filter.identity 
    |> Filter.andThen Filters.printBeforeAfterTag
    |> Filter.toService (routes |> Route.toService)

Http.host "http://+:8081/" httpService |> Async.RunSynchronously
```

Services are hosted via ```System.Net.HttpListener```.

#### Routing

```
type Route = HttpReq -> Async<HttpResp> option
```

A route is represented simlarly to a service with the exception that a response may or may not be provided depending on whether the route criteria were matched. Routes are typically declared as a combination of a match criteria and a service. Routing multiplexes HTTP requests into finer grained services. Routes form monoids and as such can be combined. Finally, given a catch-all route, such as an HTTP 404 route, routes are combined into services. 


### Bus Module

The bus module contains combinators to support various messaging patters, such as pub/sub, request/reply, query/response, fire and forget, process managers (sagas), etc.


## Central Abstractions

### Message Service

```fsharp
type MessageService<'res> = Service<Message, 'res>

type MessageService = MessageService<Message>

type MessageSink = MessageService<unit>
```

## Process Manager

```Coming soon...```


### Event Sourcing Module

```Coming soon...```