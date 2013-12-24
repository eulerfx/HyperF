namespace HyperF.Bus

open HyperF

type Message = {
    headers: Map<string, string>
    topic: string
    payloadType: System.Type
    payload: obj
}

type TransportMessage = {
    headers: Map<string, string>
    topic: string
    body: byte[] 
}

type MessageEncoder = Message -> TransportMessage

type MessageDecoder = TransportMessage -> Message


module Routing =
    
    type Route = Message -> Sink<Message> option

    module Option =
        
        let orElse (other:Lazy<option<_>>) opt = 
            match opt with
            | Some _ -> opt
            | None -> other.Force()

    let orElse (a:Route) (b:Route) : Route = fun m -> a m |> Option.orElse (lazy(b m))            

    //let orUnit (route:Route) = fun m -> route m |> Option.getOrElse (Sink.unit<Message>)
    
    module Is =
        
        open System

        let inline GTE (a:Type) (b:Type) = a.IsAssignableFrom(b)

        let inline EQ (a:Type) (b:Type) = a.Equals(b)
        

    module Match =

        /// Filter route.
        let onFilter pred (s:Sink<_>) : Route =
            fun m ->
                if pred m then s |> Some
                else None     

        let onPayloadType rel (h:Sink<'a>) : Route = onFilter (fun (m:Message) -> rel typeof<'a> m.payloadType) (fun (m:Message) -> m.payload :?> 'a |> h)
    
        /// Converts a sink to a route based on type equality.
        let onPayloadTypeEQ s = onPayloadType Is.EQ s

        /// Converts a sink to a route based on super-type relation.
        let onPayloadTypeGTE s = onPayloadType Is.GTE s


    
      




    
     

    