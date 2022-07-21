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
    
    //type HttpRequestBuilder = 
    //    {
    //        Url: string
    //        Query: (string * string) list
    //        Headers: (string * string) list
    //        Body: string
    //        ExpectsBody: bool
    //        Method: string
    //        //BAUsername: string
    //        //BAPassword: string
    //        AuthInfo: AuthInfo
    //        EvaluteResponse: (HttpResponse -> Result<obj, string>) option
    //    }
    //    static member Empty() = { ExpectsBody = false; Method="get"; Url = ""; Query = []; Headers = []; Body = ""; AuthInfo = Anon; EvaluteResponse = None }
    type MyResultCode =
        | GotData of string
        | GotBinary of byte[]
        | Failed of string

    type HttpRequestBuilder = 
        {
            Url         : string
            Query       : (string * string) list
            Headers     : (string * string) list
            Body        : string
            ExpectsBody : bool
            Method      : string
            AuthInfo    : AuthInfo
        }
        static member Empty() = { ExpectsBody = false; Method="get"; Url = ""; Query = []; Headers = []; Body = ""; AuthInfo = Anon }
    let BasicResponse = 
        (fun (x: HttpResponse) -> 
            match x.StatusCode, x.Body with
            | 200, Text x -> GotData x |> unbox |> Ok
            | 200, Binary x -> Error $"Binary code"
            | _ -> Error $"Bad code {x}"
        )
    type TypeName = TypeName of string
    type JsonText = JsonText of string

    type DeserializationError =
        | FailedToParse of (TypeName * JsonText)
        | TextWasEmpty

    type HRB2 =
        static member Create () = HttpRequestBuilder.Empty()
        static member Url x b         = {b with HttpRequestBuilder.Url = x}
        static member Query x b       = {b with HttpRequestBuilder.Query = x}
        static member Headers x b     = {b with HttpRequestBuilder.Headers = x}
        static member Body x b        = {b with HttpRequestBuilder.Body = x}
        static member ExpectsBody x b = {b with HttpRequestBuilder.ExpectsBody = x}
        static member Method x b      = {b with HttpRequestBuilder.Method = x}
        static member Auth x b        = {b with HttpRequestBuilder.AuthInfo = x }
        static member BasicAuth name password b = {b with HttpRequestBuilder.AuthInfo = BasicAuth(name, password)}
        static member SetMethodPost b = {b with Method = "post"}
        static member Run (eval: (HttpResponse -> Result<'a, 'err>)) (x: HttpRequestBuilder) : Result<'a, 'err> = 
            Http.Request (url = x.Url, query = x.Query, headers = x.Headers, httpMethod = x.Method) 
            |> eval 
        static member RunWithBasicResponse (x: HttpRequestBuilder) : Result<MyResultCode, string> = HRB2.Run BasicResponse x
        static member RunWithBasicJsonResponse (x: HttpRequestBuilder) : Result<'result, DeserializationError> = 
            HRB2.Run BasicResponse x
            |> function
            | Ok (GotData json) when json.Trim().Length = 0 -> TextWasEmpty |> Error
            | Ok (GotData json) ->
                try
                    Newtonsoft.Json.JsonConvert.DeserializeObject<'result> json |> Ok
                with ex -> 
                    $"Failed to deserialize to type {typeof<'result>.Name} {json}" |> Error
            | _ -> "" |> Error
    
    type SampleRecord =
        {
            Card: string
            Size: int
        }

    HRB2.Create()
    |> HRB2.Url ""
    |> fun x -> 
        x
        |> HRB2.RunWithBasicJsonResponse<SampleRecord>
        |> function
        | Ok (x: SampleRecord) -> ()
        | Error x -> ()

        x
    |> fun x -> 
        x |> (HRB2.RunWithBasicResponse >> function Ok x -> () | Error x -> ())
        x
    |> HRB2.Run (fun (resp: HttpResponse) -> 
                    let r : Result<HttpResponseErrors, string> = HttpResponseErrors.Throttle |> Ok
                    r
    )
    |> fun x -> x
    |> ignore
        //static member BAPassword x b = {b with HttpRequestBuilder.BAPassword = x}
    
    type HttpRequestBuilderClass<'a>() = 
        member val Url = ""                                                             with get, set
        member val Query : (string * string) list = list.Empty                          with get, set
        member val Headers: (string * string) list = list.Empty                         with get, set
        member val Body: string = ""                                                    with get, set
        member val ExpectsBody: bool = false                                            with get, set
        member val Method: string = "get"                                               with get, set
        member val AuthInfo: AuthInfo = AuthInfo.Anon                                   with get, set
        member val EvaluteResponse: (HttpResponse -> Result<'a, string>) option = None with get, set
        //static member Empty() = { ExpectsBody = false; Method="get"; Url = ""; Query = []; Headers = []; Body = ""; AuthInfo = Anon; EvaluteResponse = None }

    type HRB =
        static member Create() : HttpRequestBuilderClass<'a> = new HttpRequestBuilderClass<'a>()
        static member Url (x: string) (b: HttpRequestBuilderClass<_>): HttpRequestBuilderClass<'a> = b.Url <- x; b
        static member EvaluteResponse x (b: HttpRequestBuilderClass<_>) = b.EvaluteResponse <- Some x; b
        static member Query x (b: HttpRequestBuilderClass<_>) = b.Query <- x
        
        static member Headers x (b: HttpRequestBuilderClass<_>) = b.Headers <- x
        static member Body x (b: HttpRequestBuilderClass<_>) = b.Body <- x
        static member ExpectsBody x  (b: HttpRequestBuilderClass<_>) = b.ExpectsBody <- false
        static member Method x (b: HttpRequestBuilderClass<_>) = b.Method <- "get"
        static member Auth x (b: HttpRequestBuilderClass<_>) = b.AuthInfo <- AuthInfo.Anon
        static member BasicAuth username password (b: HttpRequestBuilderClass<_>) = b.AuthInfo <- BasicAuth(username, password)
        static member SetMethodPost (b: HttpRequestBuilderClass<_>) = b.Method <- "post"
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

    