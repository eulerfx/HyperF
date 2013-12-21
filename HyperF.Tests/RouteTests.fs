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
            
        Get("/get") => fun _ -> HttpRes.plainText "get"

        Post("/post") => fun _ -> HttpRes.plainText "post"

        Sub("/sub") ==> [
            
            Get("/leaf") => fun _ -> HttpRes.plainText "/sub/leaf"

        ]    

        All => (fun _ -> HttpRes.plainText "*")
    ] 


    let shouldMatchGetRoutePattern() =
        
        let matcher = Match.get

        let req = Fake.requestOfMethodUriStr "GET" "http://blurocket.com/get"
        let ri = RouteInfos.parse req

        let service = routes |> Route.toService

        let result = req |> service |> Fake.readResText

        assert (result = "get")
        ()


