module BusTests

open System
open Xunit

open HyperF
open HyperF.Bus

module RoutingTests =
    
    open System
 
    type IQuery<'res> = interface end

    type SomeResult = SomeResult

    type SomeQuery() = class end with interface IQuery<SomeResult>

    type SomeDerivedQuery() = inherit SomeQuery()
    
    type SomeOtherQuery() = class end with interface IQuery<string>
        

    [<Fact>]
    let ``Should Match Type By Generic Interface``() =
        let isOfGenericInterfaceType = Routing.Is.ofGenericInterfaceType (typedefof<IQuery<obj>>)
        let typ = SomeDerivedQuery().GetType()
        Assert.True(isOfGenericInterfaceType typ)
        //Assert.False(isOfGenericInterfaceType (typeof<SomeOtherQuery>))
    


module ServiceTests =
    

    type TestReq = { request : string list }

    type TestRes = { response : string list }

    [<Fact>]
    let ``Filter Should Wrap Service``() =
        
        let f : Filter<TestReq, TestRes> = 
            Filter.fromMaps 
                (fun req -> { req with request = "before" :: req.request } |> Async.returnM) 
                (fun res -> { res with response = "after" :: res.response } |> Async.returnM)

        let s : Service<TestReq, TestRes> = 
            fun req -> { response = "response" :: req.request } |> Async.returnM

        let filteredService = Filter.toService s f

        let req = { request = [ "request" ] }
        let res = req |> filteredService |> Async.RunSynchronously

        Assert.True([ "after" ; "response" ; "before" ; "request" ] = res.response)


    [<Fact>]
    let ``Filters Should Compose``() =
        
        let f1 : Filter<TestReq, TestRes> = 
            Filter.fromMaps 
                (fun req -> { req with request = "f1.before" :: req.request } |> Async.returnM) 
                (fun res -> { res with response = "f1.after" :: res.response } |> Async.returnM)

        let f2 : Filter<TestReq, TestRes> = 
            Filter.fromMaps 
                (fun req -> { req with request = "f2.before" :: req.request } |> Async.returnM) 
                (fun res -> { res with response = "f2.after" :: res.response } |> Async.returnM)

        let s : Service<TestReq, TestRes> = 
            fun req -> { response = "response" :: req.request } |> Async.returnM

        let filteredService = Filter.toService s (Filter.andThen f1 f2)

        let req = { request = [ "request" ] }
        let res = req |> filteredService |> Async.RunSynchronously

        Assert.True([ "f2.after" ; "f1.after" ; "response" ; "f1.before" ; "f2.before" ; "request" ] = res.response)



module QueryTests =

    type ProductQuery = {
        query : string
    } 

    with interface IQuery<ProductQueryResults>

    and ProductQueryResults = {
        products : string[]
    }

    let productQueryEchoService : QueryService<ProductQuery, ProductQueryResults> =
        fun query -> Async.returnM ({ ProductQueryResults.products = [| query.query |] })

    [<Fact>]        
    let ``Query Service Should Create Corresponding Route``() =
        let r = Query.asRoute productQueryEchoService
        let ms = Routing.toService r
        let query = "hello world"
        let res = Bus.query ms ({ query = query }) |> Async.RunSynchronously
        Assert.Equal<String>(query, res.products.[0])





        
        


    
