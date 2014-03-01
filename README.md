HyperF
======

F# async service framework inspired in part by [Your server as a function](http://monkey.org/~marius/funsrv.pdf). The framework provides combinators for implementing various service interaction patterns.

## Central abstractions

### Service

```
type Service<'Req, 'Res> = 'Req -> Async<'Res>
```

### Filter

```
type Filter<'Req, 'ReqInner, 'ResInner, 'Res> = 'Req -> Service<'ReqInner, 'ResInner> -> Async<'Res>
```

Filters can be interpretted as Kleisli arrows onto the continuation monad and therefore they composed via Kleisli composition.


## Modules

### HTTP Module

The HTTP module contains combinators for an HTTP service carried by ```System.Net.Http```.

```
#r "HyperF.dll"

open Http
open Route

let routes = [
     
    Get("/get_plain_text/:id") => fun _ -> HttpRes.plainText "hello world"

    Get("/echo_id/:id") => (^) (fun (req,ri,model:int) -> HttpRes.plainText (sprintf "id=%i" model))

    Post("/get_v3/:id") => 
        fun _ -> async {
            do! Async.Sleep(1000)
            return! HttpRes.plainText "post" }

    Sub("/sub") ==> [
    
        Get("/nested") => fun _ -> HttpRes.plainText "/sub/nested"

    ]    

    All => (fun _ -> HttpRes.plainText "*")
] 

let routeService = routes |> Route.toService

let service = 
    Filter.identity 
    |> Filter.andThen Filter.printBeforeAfterTag
    |> Filter.toService routeService

Http.host "http://+:8081/" service |> Async.RunSynchronously
```

Services are hosted via ```System.Net,HttpListener```.

### Bus Module

The bus module contains combinators to support various messaging patters, such as pub/sub, request/reply, query/response, fire and forget, process managers (sagas).


## Central Abstractions

### Message Service

```
type MessageService<'res> = Service<Message, 'res>

type MessageService = MessageService<Message>

type MessageSink = MessageService<unit>
```

## Process Manager

```Coming soon...```


### Event Sourcing Module

```Coming soon...```