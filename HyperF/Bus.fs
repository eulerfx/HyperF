namespace HyperF.Bus

open HyperF

type Message = {
    headers: Map<string, string>
    topic: string
    payloadType: System.Type
    payload: obj
}

type MessageService<'res> = Service<Message, 'res>

type MessageService = MessageService<Message>

type MessageSink = MessageService<unit>



type Sink<'Req> = Service<'Req, unit>

module Sink =

    module Seq =
        
        let apply s fs = fs |> Seq.map (fun f -> f s) 
    
    [<GeneralizableValue>]
    let unit<'a> : Sink<'a> = 
        let asyncUnit = Async.returnA()
        fun req -> asyncUnit

    let combine (ss:Sink<_> seq) = fun req -> Async.Parallel(ss |> Seq.apply req) |> Async.Ignore

    let append (a:Sink<_>) (b:Sink<_>) = 
        fun req -> async {
            let! a = a req |> Async.StartChild
            let! b = b req |> Async.StartChild
            do! a
            do! b }




type Route<'res> = Message -> MessageService<'res> option


module Routing =           

    module Option =
        
        let orElse (other:option<_>) opt = 
            match opt with
            | Some _ -> opt
            | None -> other

    let identity : Route<_> = fun _ -> None

    let append (a:Route<_>) (b:Route<_>) : Route<_> = fun m -> a m |> Option.orElse (b m)

    let toService (r:Route<'res>) : MessageService<'res> =
        
        let failService : MessageService<'res> = 
            fun msg ->
                failwith (sprintf "Unable to find handler for message type %O" msg.payloadType)
                Async.returnM (Unchecked.defaultof<'res>)

        fun msg -> 
            let service = msg |> r |> Option.getOrElse failService
            msg |> service

    module Match =

        let onPred pred (s:MessageService<'res>) : Route<'res> =
            fun m ->
                if pred m then s |> Some
                else None     

        let onPayloadType pred (s:MessageService<'res>) : Route<'res> = onPred (fun (m:Message) -> pred m.payloadType) s


    module Is =
        
        open System

        let inline GTE (a:Type) (b:Type) = a.IsAssignableFrom(b)

        let inline EQ (a:Type) (b:Type) = a.Equals(b)

        // TODO: cache
        let ofGenericInterfaceType (genericInterfaceType:Type) =
            let isOf (typ:Type) =
                typ.GetInterfaces()
                |> Seq.exists (fun intr -> if intr.IsGenericType && intr.GetGenericTypeDefinition() = genericInterfaceType then true else false)
            isOf




module Transport =
    
    /// A message in the transport layer.
    type TransportMessage = {
        headers : Map<string, string>
        topic   : string
        replyTo : string
        body    : byte[] 
    }

    type Encoder = Message -> TransportMessage

    type Decoder = TransportMessage -> Message







type IQuery<'TResult> = interface end

type QueryService<'Query, 'Res when 'Query :> IQuery<'Res>> = Service<'Query, 'Res>

module Query =
    
    let private toMessageService (s:QueryService<'Query, 'Res>) : MessageService = 
        fun (msg:Message) -> async {
            let query = msg.payload :?> 'Query
            let! res = query |> s
            return {
                Message.headers = Map.empty
                topic = ""
                payloadType = typeof<'Res>
                payload = res
            }
        }

    let asRoute (s:QueryService<'Query, 'Res>) =     
        let msgService = s |> toMessageService
        let isQuery = Routing.Is.EQ (typeof<'Query>)
        Routing.Match.onPayloadType isQuery msgService




/// A transducer (saga = workflow = process manager = aggregate = projection)
type Transducer<'State, 'In, 'Out> = {
    transition : 'In -> 'State -> ('State * 'Out) // state monad
    zero       : 'State
    apply      : 'Out -> 'State -> 'State
}

module Transducer =
    
    let asMessageSink = 0




module Bus =    

    /// Creates a message from an object based on configured parameters
    let private toMsg obj =        
        { 
            headers = Map.empty
            topic = ""
            payloadType = obj.GetType()
            payload = box obj
        }

    let send (s:MessageSink) (cmd:obj) = async {
        let msg = toMsg cmd
        do! s msg            
    }

    let publish (s:MessageSink) (evt:obj) = async {
        let msg = toMsg evt
        do! s msg            
    }

    let query (s:MessageService) (q:IQuery<'res>) = async {
        let msg = q |> toMsg
        let! res = msg |> s
        return res.payload :?> 'res
    }

    // handle command = subscribe to event = register query handler






    
     

    