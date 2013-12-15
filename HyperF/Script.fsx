#r "System.Net.Http"

#load "Prelude.fs"

open HyperF

#load "Future.fs"

open HyperF

#load "Service.fs"

open HyperF

#load "IO.fs"

open HyperF

#load "Http.fs"

open HyperF

Http.host (HttpServices.helloWorld) |> Async.RunSynchronously

