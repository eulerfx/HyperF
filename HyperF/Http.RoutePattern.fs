namespace HyperF

open System
    
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


module RoutePattern =            

    type PathPattern = 
        | Segment of segment:string
        | Var     of name:string
        | Any

    and QueryPattern = Param of name:string * valueName:string

    let private splitSegments = Strings.splitByCharRE '/'
    let private stripQualifier str = str |> Strings.trimStart '{' |> Strings.trimEnd '}'        

    let private parseQueryPattern (queryPat:string) =
        queryPat 
        |> Query.parse
        |> Map.toSeq
        |> Seq.map (fun (name,values) -> Param(name,values.[0] |> stripQualifier))

    let private parseSegmentPattern (pat:string) =
        pat
        |> splitSegments
        |> Array.map (fun segment -> 
            if segment.StartsWith(":") then 
                let var = segment.TrimStart(':')
                Var(var)
            elif segment = "*" then Any
            else Segment segment
        )       

    let matchUrl (pat:string) =        

        let pts = pat |> Strings.splitByChar '?'
        let patternSegments = parseSegmentPattern pts.[0]
        let queryPattern = if pts.Length > 1 then parseQueryPattern pts.[1] else Seq.empty

        fun (url:Uri) ->

            let urlSegments = url.AbsolutePath |> splitSegments
                    
            let matchSegment (pat,urlSegment) =
                match pat with
                | Segment segment -> Strings.equalToIgnoreCase segment urlSegment,None
                | Var name        -> true,Some(name,urlSegment)
                | Any             -> true,None            
            
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

            if (Array.length patternSegments) = (segmentMatchCount) && (Seq.length queryPattern) = (Array.length queryMatches) then Some(values)
            else None

