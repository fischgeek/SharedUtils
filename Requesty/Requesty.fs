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

    type UserName = Username of string
    type Password = Password of string
    type BearerInfo = BearerInfo of string
    type AuthInfo =
        | BasicAuth of UserName * Password
        | Bearer of BearerInfo
        | Anon
    
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
    type TypeName = TypeName of string
    type JsonText = JsonText of string

    type DeserializationError =
        | FailedToParse of (TypeName * JsonText)
        | TextWasEmpty
        | GenericError of string

    let private PrepRequest (x: HttpRequestBuilder) =
        let x = 
            match x.AuthInfo with
            | AuthInfo.BasicAuth (Username u, Password p) -> {x with Headers = x.Headers @ ["username", u; "password", p]}
            | _ -> x
        x
    type HRB =
        static member Create () = HttpRequestBuilder.Empty()
        
        //static member FormattedURL (url: string) (pathPart: string) (params: string list) =
        //    params
        //    |> SP.Join "&" // name=newname&desc=newdesc&pos=2
        //    |> SP.PrependIfNotEmpty "?" // ?name=newname&desc=newdesc&pos=2
        //    |> fun params -> 
        //        (
        //            url.Trim("/")
        //            path.Trim("/")
        //        )
        //        |> SP.Join "/"
        //        + params
        //    $"{url}//1{url}" // https://api.trello.com/1?name=newname&desc=newdesc&pos=2
        //    |> HRB.Url

        static member Url x b         = {b with HttpRequestBuilder.Url = x}
        static member Query x b       = {b with HttpRequestBuilder.Query = x}
        static member Headers x b     = {b with HttpRequestBuilder.Headers = x}
        static member Body x b        = {b with HttpRequestBuilder.Body = x}
        static member ExpectsBody x b = {b with HttpRequestBuilder.ExpectsBody = x}
        static member Method x b      = {b with HttpRequestBuilder.Method = x}
        static member SetMethodPost b = {b with Method = "post"}
        static member Run (interpreter: (HttpResponse -> Result<'a, 'err>)) (x: HttpRequestBuilder) : Result<'a, 'err> = 
            let x = PrepRequest x
            Http.Request (url = x.Url, query = x.Query, headers = x.Headers, httpMethod = x.Method) 
            |> interpreter 
        static member StockFns = new StockFns()       
        static member StockInterpreters = new StockInterpreters()
        static member Auth = new Auth()
    
    and StockInterpreters() =
        member _.TextInterpreter (x: HttpResponse) =
                match x.StatusCode, x.Body with
                | 200, Text x -> x |> Ok
                | 200, Binary x -> Error $"Binary code"
                | _ -> Error $"Bad code {x}"
        
        member _.JSONInerpreter<'dataStructure> (x: Result<string, string>) : Result<'dataStructure, DeserializationError> =
            x
            |> function
            | Ok json when json.Trim().Length = 0 -> TextWasEmpty |> Error
            | Ok json ->
                try
                    Newtonsoft.Json.JsonConvert.DeserializeObject<'dataStructure> json |> Ok
                with ex -> 
                    $"Failed to deserialize to type {typeof<'dataStructure>.Name} {json}" |> DeserializationError.GenericError |> Error
            | Error someError -> $"Who knows: {someError}" |> GenericError |> Error
    
    and Auth() =
        member _.Basic name password b = {b with HttpRequestBuilder.AuthInfo = BasicAuth(name, password)}

    and StockFns() =
        member _.RunWithTextResponse (x: HttpRequestBuilder) : Result<string, string> = HRB.Run HRB.StockInterpreters.TextInterpreter x
       
        member _.RunWithBasicJsonResponse (x: HttpRequestBuilder) : Result<'dataStructure, DeserializationError> = 
            HRB.Run HRB.StockInterpreters.TextInterpreter x
            |> HRB.StockInterpreters.JSONInerpreter<'dataStructure>
    
    type SampleRecord =
        {
            Card: string
            Size: int
        }

    HRB.Create()
    |> HRB.Url ""
    |> HRB.Auth.Basic (Username "") (Password "")
    |> fun x -> 
        x
        |> HRB.StockFns.RunWithBasicJsonResponse<SampleRecord>
        |> function
        | Ok (x: SampleRecord) -> ()
        | Error x -> ()

        x
    |> fun x -> 
        x |> (HRB.StockFns.RunWithTextResponse >> function Ok x -> () | Error x -> ())
        x
    //|> HRB2.Run (fun (resp: HttpResponse) -> 
    //                let r : Result<HttpResponseErrors, string> = HttpResponseErrors.Throttle |> Ok
    //                r
    //)
    |> fun x -> x
    |> ignore
        //static member BAPassword x b = {b with HttpRequestBuilder.BAPassword = x}

    //let makeCall endpoint params =
    //    HRB.Create()
    //    |> HRB.FormattedURL "trellowhatever/api/" endpoint params

    //makeCall "CreateCard" []
    //|> HRB.Run