namespace Requesty

open FSharp
open FSharp.Data
open FSharp.Data.HttpRequestHeaders

module RequestyApplied = 
    let eval = 
        (fun (x: HttpResponse) -> 
            match x.StatusCode, x.Body with
            | 200, Text x -> GotData x |> unbox |> Ok
            | 200, Binary x -> Error $"Binary code"
            | _ -> Error $"Bad code {x}"
        )
    //let resp  = eval {HttpResponse.Body = Text ""; HttpResponse.StatusCode = 200; HttpResponse.Headers = Map.empty; HttpResponse.Cookies = Map.empty; ResponseUrl = ""}
    //let resp : Result<MyResultCode, string> = eval {HttpResponse.Body = Text ""; HttpResponse.StatusCode = 200; HttpResponse.Headers = Map.empty; HttpResponse.Cookies = Map.empty; ResponseUrl = ""}
    let newCall = 
        HRB.Create<MyResultCode>()
        |> (fun x -> x)
        |> HRB.Url @"www."
        |> HRB.EvaluteResponse eval
        |> HRB.Run
        |> fun x -> x
        |> function
        | Ok x -> 
            x
            |> function
            | GotData x -> ()
            | GotBinary x -> ()
            | Failed x -> ()
        | Error str -> ()
            
        let myCreate = HRB.Create<MyResultCode>

        myCreate()
        |> HRB.Run
        |> (fun x -> x)

    //let MakeCall verb url fn msg =
    //    HRB.Url url
    //    |> HRB.Body ""
    //    |> verb
    //    |> function
    //    | Success x -> fn x
    //    | _ -> failwith msg
    
    //let MakeCallWithBody verb url (body:string) fn msg = 
    //    HRB.Url url
    //    |> HRB.Body body
    //    |> verb
    //    |> function
    //    | Success x -> fn x
    //    | _ -> failwith msg

    //let MakeCallWithBodyBA verb url (body:string) fn msg username password = 
    //    HRB.Url url
    //    |> HRB.BAUsername username
    //    |> HRB.BAPassword password
    //    |> HRB.Body body
    //    |> verb
    //    |> function
    //    | Success x -> fn x
    //    | _ -> failwith msg

    //type BuilderToStatusToResult = (HttpRequestBuilder -> Result<JsonString,HttpResponseErrors>)
    
    //let MakeCallWithBodyBA_Result verb url (body:string) username password : Result<JsonString,PossibleWebCallErrors> = 
    //    HRB.Url url
    //    |> HRB.BAUsername username
    //    |> HRB.BAPassword password
    //    |> HRB.Body body
    //    |> verb
    //    //|> Result.mapError HttpResponseErrors

    //    //|> function
    //    //| Ok x -> x |> Ok
    //    //| Error x -> x |> HttpResponseErrors |> Error

    //let MakeCallWithBA verb url fn msg username password = 
    //    HRB.Url url
    //    |> HRB.BAUsername username
    //    |> HRB.BAPassword password
    //    |> verb
    //    |> function
    //    | Success x -> fn x
    //    | _ -> failwith msg

    //let MakeCallWithBA_Result verb url msg username password = 
    //    HRB.Url url
    //    |> HRB.BAUsername username
    //    |> HRB.BAPassword password
    //    |> verb
    //    |> function
    //    | Success x -> x |> Ok
    //    | _ -> FailedForSomeReason msg |> Error

    //let MakeCallWithBearerToken verb url (token: string) msg = 
    //    HRB.Url url
    //    |> HRB.Headers [("Authorization",$"Bearer {token}")]
    //    |> verb
    //    |> function
    //    | Success x -> x |> Ok
    //    | _ -> FailedForSomeReason msg |> Error

    //let private returnResponse(res: HttpResponse): HttpResponseStatus =
    //    match res.StatusCode with
    //    | 401 -> Unauthorized
    //    | 429 -> Throttle
    //    | _ ->
    //        match res.Body with
    //        | Text txt -> Success txt
    //        | _ -> Nothing

    //let private returnResponse2 (res: HttpResponse): Result<JsonString, HttpResponseErrors> =
    //    match res.StatusCode with
    //    | 401 -> HttpResponseErrors.Unauthorized |> Error
    //    | 429 -> HttpResponseErrors.Throttle |> Error
    //    | _ ->
    //        match res.Body with
    //        | Text txt -> JsonString txt |> Ok
    //        | _ -> HttpResponseErrors.Nothing |> Error
    
    //let adapt (fn: HttpRequestBuilder -> HttpResponse) : (HttpRequestBuilder -> Result<JsonString, PossibleWebCallErrors>) =
    //    fun hrb -> 
    //        try
    //            fn hrb
    //            |> returnResponse2
    //            |> Result.mapError HttpResponseErrors
    //        with ex -> VerbFailedWithException ex |> Error

    //let HttpGet2 = adapt (fun (x:HttpRequestBuilder) -> Http.Request(url = x.Url, query = x.Query, headers = x.Headers, httpMethod = "get") )
    //let HttpPostJsonWithBasicAuth2 = adapt (fun (x:HttpRequestBuilder) -> Http.Request(url = x.Url, query = x.Query, headers = [ ContentType HttpContentTypes.Json; BasicAuth x.BAUsername x.BAPassword ], httpMethod = "post", body = (x.Body |> TextRequest)))


    //// legacy
    
    //let HttpGet (x:HttpRequestBuilder) = 
    //    Http.Request(url = x.Url, query = x.Query, headers = x.Headers, httpMethod = "get") |> returnResponse
    
    //let HttpPut (x:HttpRequestBuilder) = 
    //    Http.Request(url = x.Url, query = x.Query, headers = x.Headers, httpMethod = "put") |> returnResponse
    
    //let HttpDel (x:HttpRequestBuilder) = 
    //    Http.Request(url = x.Url, query = x.Query, headers = x.Headers, httpMethod = "delete") |> returnResponse
    
    //let HttpPost (x:HttpRequestBuilder) =
    //    Http.Request (url = x.Url, query = x.Query, headers = x.Headers, httpMethod = "post") |> returnResponse

    //let HttpPostJson (x:HttpRequestBuilder) =
    //    Http.Request (url = x.Url, query = x.Query, headers = [ ContentType HttpContentTypes.Json ], httpMethod = "post", body = (x.Body |> TextRequest)) |> returnResponse

    //let HttpPostJsonWithBasicAuth (x:HttpRequestBuilder) = 
    //    Http.Request(url = x.Url, query = x.Query, headers = [ ContentType HttpContentTypes.Json; BasicAuth x.BAUsername x.BAPassword ], httpMethod = "post", body = (x.Body |> TextRequest)) |> returnResponse


    //let HttpPost2 (x:HttpRequestBuilder) =
    //    Http.Request (url = x.Url, query = x.Query, headers = [ ContentType HttpContentTypes.Text ], httpMethod = "post", body = (x.Body |> TextRequest)) |> returnResponse

    //let HttpPutJson (x:HttpRequestBuilder) = 
    //    Http.Request(url = x.Url, query = x.Query, headers = [ ContentType HttpContentTypes.Json ], httpMethod = "put", body = (x.Body |> TextRequest)) |> returnResponse

    //let HttpPutJsonWithBasicAuth (x:HttpRequestBuilder) = 
    //    Http.Request(url = x.Url, query = x.Query, headers = [ ContentType HttpContentTypes.Json; BasicAuth x.BAUsername x.BAPassword ], httpMethod = "put", body = (x.Body |> TextRequest)) |> returnResponse

    //let HttpGetWithBasicAuth (x:HttpRequestBuilder) =
    //    Http.Request(url = x.Url, query = x.Query, headers = [ ContentType HttpContentTypes.Json; BasicAuth x.BAUsername x.BAPassword ], httpMethod = "get") |> returnResponse

    //let Get fn msg url = MakeCall HttpGet url fn msg
    //let GetWithBA fn msg username password url = 
    //    HRB.Url url
    //    |> HRB.BAUsername username
    //    |> HRB.BAPassword password
    //    |> HttpGetWithBasicAuth
    //    |> function
    //    | Success x -> fn x
    //    | _ -> failwith msg
    //let Put fn msg url = MakeCall HttpPut url fn msg
    //let PutJson fn msg body url = MakeCallWithBody HttpPutJson url body fn msg
    //let PutJsonWithBA fn msg username password body url = MakeCallWithBodyBA HttpPutJsonWithBasicAuth url body fn msg username password
    //let Del fn msg url = MakeCall HttpDel url fn msg
    //let Pos fn msg url = MakeCall HttpPost url fn msg

