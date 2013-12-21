namespace HyperF

open Xunit

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


