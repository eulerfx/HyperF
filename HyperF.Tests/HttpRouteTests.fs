namespace HyperF

open Xunit
open FsUnit.Xunit
    

module Fake =

    open System
    open System.Text

    let requestOfMethodUriStr httpMethod uriString = 
        { headers = Map.empty
          httpMethod = httpMethod
          url = Uri(uriString)
          body = null }

    let resOfText text = HttpRes.plainText text

    let readResText (res:Async<HttpResp>) = 
        async { let! res = res in return res.body |> IO.readToString |> Async.RunSynchronously } |> Async.RunSynchronously
        


module RouteInfoTests = 
    
    open Route
    open EkonBenefits.FSharp.Dynamic

    [<Fact>]
    let shouldMerge() =
        let ri = { values = new System.Dynamic.ExpandoObject() }
        let ri = ri.mergeMap([ ("hello","world") ; ("foo","bar") ] |> Map.ofList)
        ri.values?hello |> should equal "world"
        ri.values?foo |> should equal "bar"
        ri.getValue("foo") |> should equal "bar"


module MatchTests =

    open Route

    let routes = [
            
        Get("/get") => fun _ -> HttpRes.plainText "/get"

        Post("/post") => fun _ -> HttpRes.plainText "/post"

        Sub("/sub") ==> [
            
            Get("/leaf") => fun _ -> HttpRes.plainText "/sub/leaf"

        ]    

        All => (fun _ -> HttpRes.plainText "/*")
    ] 

    let service = routes |> Route.toService

    let res req = req |> service |> Fake.readResText
    let req = Fake.requestOfMethodUriStr "GET" "http://blurocket.com/get"

    let test expect req = Assert.Equal<string>(expect, res req)

    [<Fact>]
    let ``/get``() = test "/get" (Fake.requestOfMethodUriStr "GET" "http://blurocket.com/get")

    [<Fact>]
    let ``/post``() = test "/post" (Fake.requestOfMethodUriStr "POST" "http://blurocket.com/post")

    [<Fact>]
    let ``/sub/leaf``() = test "/sub/leaf" (Fake.requestOfMethodUriStr "GET" "http://blurocket.com/sub/leaf")

    [<Fact>]
    let ``/*``() = test "/*" (Fake.requestOfMethodUriStr "GET" "http://blurocket.com/blahhh")       



module PatternTests =
    
    open System
    open Route
    open RoutePattern

    // /users/:id
    // /users/:id|int
    // /users/:id|int?
    // /users/:id|regex:\d{3,}
    // /users/{id|regex:\d{3,}}
    // /users/{id|?}
    // /users/:id|?              
    // /users/{id}
    // /users/{id}?q={query}

    let url1 = Uri("http://blur.com/users/123?format=json")

    let shouldMatch pat url = RoutePattern.matchUrl pat (Uri(url)) |> Option.isSome |> should be True

    let shouldMatchValues pat url (values:(string * string) seq) = 
        
        let matchValues = RoutePattern.matchUrl pat (Uri(url)) |> Option.get

        let matchValues = matchValues |> Map.toSeq |> Seq.sort
        let x = Seq.compareWith (Operators.compare) matchValues (values |> Seq.sort)

        x |> should equal 0

    [<Fact>]
    let shoutMatchSimple() = shouldMatch "/users" "http://blur.com/users"

    [<Fact>]
    let shoutMatchWithSegment() = shouldMatch "/users/:id" "http://blur.com/users/123"

    [<Fact>]
    let shoutMatchWithSegmentAndQueryParameter() = shouldMatch "/users/:id?format={format}" "http://blur.com/users/123?format=json"

    [<Fact>]
    let shoutMatchValuesWithSegmentAndQueryParameter() = 
        shouldMatchValues "/users/:id?format={format}" "http://blur.com/users/123?format=json" [ ("id","123") ; ("format","json") ]
        

        
