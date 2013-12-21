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

    let splitSegments = Strings.splitByCharRE '/'
    
    module Query =

        let parse (queryStr:string) =
            if String.IsNullOrEmpty(queryStr) then Map.empty
            else 
                queryStr
                |> Strings.trimStart '?'
                |> Strings.splitByChar '&'
                |> Array.map (fun pair -> let pts = pair |> Strings.splitByChar '=' in (pts.[0],pts.[1]))
                |> Seq.groupBy fst
                |> Seq.map (fun (k,pairs) -> k,pairs |> Seq.map snd |> Seq.toArray)
                |> Map.ofSeq    

        let param param (query:Map<string,string[]>) = let values = query |> Map.find param in values.[0]

        let tryParam param (query:Map<string,string[]>) = 
            query |> Map.tryFind param |> Option.map (fun vs -> vs.[0])


    type UrlTemplate = PathPattern list * QueryPattern list

    and PathPattern = 
        | Path of segment:string
        | Var  of name:string
        | Any

    and QueryPattern = Param of name:string * valueName:string

    module Pattern =            

        let private stripQualifier str = str |> Strings.trimStart '{' |> Strings.trimEnd '}'        

        let parseQueryPattern (queryPat:string) =
            queryPat 
            |> Query.parse
            |> Map.toSeq
            |> Seq.map (fun (name,values) -> Param(name,values.[0] |> stripQualifier))



        let parse (pat:string) =
            pat
            |> splitSegments
            |> Array.map (fun pt -> 
                if pt.StartsWith(":") then 
                    let var = pt.TrimStart(':')
                    Var(var)
                elif pt = "*" then Any
                else Path pt
            )
       

        let matchUrl (pat:string) (uriString:string) =        

            let pts = pat |> Strings.splitByChar '?'
            let patternSegments = parse pts.[0]
            let queryPattern = if pts.Length > 1 then parseQueryPattern pts.[1] else Seq.empty

            let url = Uri(uriString)           

            let urlSegments = url.AbsolutePath |> splitSegments
                    
            let matchSegment (pat,segment) =
                match pat with
                | Path path -> Strings.equalToIgnoreCase path segment,None
                | Var name  -> true,Some(name,segment)
                | Any       -> true,None            
            
            let segmentMatches = 
                Seq.zip patternSegments urlSegments 
                |> Seq.map matchSegment
                |> Seq.where (fun (m,value) -> m = true)

            let segmentMatchCount = segmentMatches |> Seq.length

            let segmentMatches =
                segmentMatches
                |> Seq.map snd
                |> Option.join
                |> Seq.toArray

            let query = url.Query |> Query.parse

            let queryMatches = 
                queryPattern 
                |> Seq.map (fun (Param(name,valueName)) -> query |> Query.tryParam name |> Option.map (fun value -> (valueName,value)))
                |> Option.join
                |> Seq.toArray
            
            let values = 
                Seq.append segmentMatches queryMatches
                |> Map.ofSeq

            (Array.length patternSegments) = (segmentMatchCount) && (Seq.length queryPattern) = (Array.length queryMatches)
            ,values




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

    let shouldMatch pat url = Pattern.matchUrl pat url |> fst |> should be True

    let shouldMatchValues pat url (values:(string * string) seq) = 
        
        let matched,matchValues = Pattern.matchUrl pat url
        //matched |> should equal True        
        Assert.True(matched)

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
        

        
