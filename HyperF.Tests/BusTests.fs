module BusTests

open Xunit
open HyperF.Bus

module RoutingTests =
    
    open System
 
    type IQuery<'res> = interface end

    type SomeResult = SomeResult

    type SomeQuery() = class end with interface IQuery<SomeResult>

    type SomeDerivedQuery() = inherit SomeQuery()
        

    [<Fact>]
    let ``Should Match Type By Generic Interface``() =
        let isOfGenericInterfaceType = Routing.Is.ofGenericInterfaceType (typedefof<IQuery<obj>>)
        let typ = SomeDerivedQuery().GetType()
        let flag = isOfGenericInterfaceType typ
        Assert.True(flag)
    


module QueryTests =

    type ProductQuery = {
        query : string
    } 

    with interface IQuery<ProductQueryResults>

    and ProductQueryResults = {
        products : string[]
    }
