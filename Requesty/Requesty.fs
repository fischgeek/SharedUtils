namespace Requesty

open FSharp.Data
open FSharp.Data.HttpRequestHeaders

[<AutoOpen>]
module Requesty =
    type JsonString = JsonString of string

    [<RequireQualifiedAccess>]
    type HttpResponseErrors =
        | Unauthorized
        | Throttle
        | Nothing
        static member Stringify x = x.ToString()

    type PossibleWebCallErrors = 
        | FailedForSomeReason of string
        | VerbFailedWithException of exn
        | HttpResponseErrors of HttpResponseErrors
        static member Stringify = 
            function
            | FailedForSomeReason x -> $"The web call failed. {x}"
            | x -> x.ToString()

    type UserName = UserName of string
    type Password = Password of string
    type BearerInfo = BearerInfo of string
    type AuthInfo =
        | BasicAuth of UserName * Password
        | Bearer of BearerInfo
        | Anon
    
    type HttpRequestBuilder = 
        {
            Url: string
            Query: (string * string) list
            Headers: (string * string) list
            Body: string
            ExpectsBody: bool
            Method: string
            //BAUsername: string
            //BAPassword: string
            AuthInfo: AuthInfo
            EvaluteResponse: (HttpResponse -> Result<obj, string>) option
        }
        static member Empty() = { ExpectsBody = false; Method="get"; Url = ""; Query = []; Headers = []; Body = ""; AuthInfo = Anon; EvaluteResponse = None }

    type HttpRequestBuilderClass<'a>() = 
        member val Url = ""                                                             with get, set
        member val Query : (string * string) list = list.Empty                          with get, set
        member val Headers: (string * string) list = list.Empty                         with get, set
        member val Body: string = ""                                                    with get, set
        member val ExpectsBody: bool = false                                            with get, set
        member val Method: string = "get"                                               with get, set
        member val AuthInfo: AuthInfo = AuthInfo.Anon                                   with get, set
        member val EvaluteResponse: (HttpResponse -> Result<'a, string>) option = None with get, set
        static member Empty() = { ExpectsBody = false; Method="get"; Url = ""; Query = []; Headers = []; Body = ""; AuthInfo = Anon; EvaluteResponse = None }

    type HRB =
        static member Create() : HttpRequestBuilderClass<'a> = new HttpRequestBuilderClass<'a>()
        static member Url (x: string) (b: HttpRequestBuilderClass<_>): HttpRequestBuilderClass<'a> = 
            b.Url <- x
            b
        static member EvaluteResponse x (b: HttpRequestBuilderClass<_>) = 
            b.EvaluteResponse <- Some x
            b
        static member Query x (b: HttpRequestBuilderClass<_>) = b.Query <- x
        static member Headers x b = {b with HttpRequestBuilder.Headers = x}
        static member Body x b = {b with HttpRequestBuilder.Body = x}
        static member ExpectsBody x b = {b with HttpRequestBuilder.ExpectsBody = x}
        static member Method x b = {b with HttpRequestBuilder.Method = x}
        static member Auth x b = {b with HttpRequestBuilder.AuthInfo = x }
        //static member EvaluteResponse x b = {b with HttpRequestBuilder.EvaluteResponse = Some x }
        static member BasicAuth name password b = {b with HttpRequestBuilder.AuthInfo = BasicAuth(name, password)}
        static member SetMethodPost b = {b with Method = "post"}
        static member Run(x: HttpRequestBuilderClass<'a>) : Result<'a, string> = 
            Http.Request (url = x.Url, query = x.Query, headers = x.Headers, httpMethod = x.Method) 
            |> fun resp ->
                match x.EvaluteResponse with
                | Some eval -> eval resp 
                | None -> Error "asd"

        //static member BAPassword x b = {b with HttpRequestBuilder.BAPassword = x}
    
    type HttpResponseStatus =
        | Unauthorized
        | Throttle
        | Success of string
        | Nothing

    type MyResultCode =
        | GotData of string
        | GotBinary of byte[]
        | Failed of string


    
