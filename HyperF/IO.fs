namespace HyperF

open System.IO

module IO =
    
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


module SeqStream =
 
    /// Reads a specified number of values from an enumeration into a buffer array.
    let private fillBuffer (enum:System.Collections.Generic.IEnumerator<_>) (buffer:_ array) offset count =
        let mutable read = 0
        while enum.MoveNext() && read < count do
            buffer.[read + offset] <- enum.Current
            read <- read + 1
        read
 
    /// A read-only stream which wraps an enumerator of a byte sequence.
    type ByteEnumeratorStream (enumerator:System.Collections.Generic.IEnumerator<byte>) =
        inherit System.IO.Stream()
       
        let mutable position = 0L
 
        override __.Read(buffer,offset,count) = 
            let read = fillBuffer enumerator buffer offset count
            position <- position + (read |> int64)
            read
 
        override __.CanRead = true        
        override __.CanSeek = false        
        override __.CanWrite = false        
        override __.Flush() = ()
        override __.Dispose(disposing) = enumerator.Dispose()        
        override __.Length = 0L        
        override __.Position with get () = position and set value = failwith "Can't set position!"
        override __.Seek(offset, origin) = failwith "Can't seek!"
        override __.Write(bugger,offset,count) = failwith "Can't write to read-only stream!"
        override __.SetLength(value) = failwith "Can't set length!"
 
 
    /// Wraps the specified sequence of byte rows in a Stream.
    let ofByteRows rows = 
        let flatRows = rows |> Seq.concat
        new ByteEnumeratorStream(flatRows.GetEnumerator())


module AsyncSeqStream =

    open FSharp.Control
    open System.Threading.Tasks
       
    /// Reads a specified number of values from an enumeration into a buffer array.
    let private fillBufferAsync (input:AsyncSeq<_>) (buffer:_ array) offset count = async {
        let rec loop input read = async {
            if read = count then return read,input
            else 
                let! v = input
                match v with 
                | Nil -> return read,input
                | Cons (head,tail) -> 
                    buffer.[offset + read] <- head
                    return! loop tail (read + 1) }
        return! loop input 0 }


    /// A read-only stream which wraps an enumerator of a byte sequence.
    type AsyncByteEnumeratorStream (input:AsyncSeq<byte>) =
        inherit System.IO.Stream()
       
        let mutable position = 0L
        let mutable input = input

        override __.ReadAsync(buffer,offset,count,cancellationToken) = 
            let readAsync = async {
                let! read,resultInput = fillBufferAsync input buffer offset count
                input <- resultInput
                position <- position + (read |> int64)
                return read }
            Async.StartAsTask(readAsync, TaskCreationOptions.None, cancellationToken)
        
        override this.Read(buffer,offset,count) = this.ReadAsync(buffer,offset,count) |> Async.AwaitTask |> Async.RunSynchronously        

        override __.CanRead = true        
        override __.CanSeek = false        
        override __.CanWrite = false        
        override __.Flush() = ()
        override __.Dispose(disposing) = ()
        override __.Length = failwith "Seek not supported!"        
        override __.Position with get () = position and set value = failwith "Can't set position!"
        override __.Seek(offset, origin) = failwith "Can't seek!"
        override __.Write(bugger,offset,count) = failwith "Can't write to read-only stream!"
        override __.SetLength(value) = failwith "Can't set length!"


    module AsyncSeq =    

        let concat (sources:AsyncSeq<#seq<_>>) = asyncSeq { for source in sources do for item in source do yield item }

    let ofByteRows rows = 
        let flatRows = rows |> AsyncSeq.concat
        new AsyncByteEnumeratorStream(flatRows)