namespace HyperF

open System.IO

module IO =
    
    type 'a IO = IO of 'a

    let returnIO = IO

    let bind (io:'a IO) (k:'a -> 'b IO) = let (IO(value)) = io in k value

    let map f (io:'a IO) = let (IO(value)) = io in f value |> returnIO

    let apply (f:('a -> 'b) IO) (io:'a IO) = 
        let (IO(f)) = f in
        let (IO(value)) = io in
        f value |> returnIO

    //let delay (f:unit -> 'a IO) = f()

    //let append (a:'a IO) (b:'a IO) = b

    let lift f a = f a |> IO

    let lift2 f a b = f a b |> IO

    let lift3 f a b c = f a b c |> IO

    let readFileText path = File.ReadAllText(path) |> returnIO

    let copyTo (source:Stream) (sink:Stream) = source.CopyToAsync(sink) |> Async.AwaitIAsyncResult |> Async.Ignore

    let readToString (stream:Stream) = async {
        use sr = new StreamReader(stream)
        return! sr.ReadToEndAsync() |> Async.AwaitTask }
        
    type ReadStream (stream:Stream) =
        inherit System.IO.Stream()
       
        let mutable position = 0L
 
        override __.Read(buffer,offset,count) = 
            let read = stream.Read(buffer, offset, count)
            read
 
        override __.CanRead = true        
        override __.CanSeek = false        
        override __.CanWrite = false        
        override __.Flush() = ()
        override __.Dispose(disposing) = stream.Dispose()        
        override __.Length = stream.Length        
        override __.Position with get () = position and set value = failwith "Can't set position!"
        override __.Seek(offset, origin) = failwith "Can't seek!"
        override __.Write(bugger,offset,count) = failwith "Can't write to read-only stream!"
        override __.SetLength(value) = failwith "Can't set length!"  

    let echo (stream:Stream) = new ReadStream(stream)
