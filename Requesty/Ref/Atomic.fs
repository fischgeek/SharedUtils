// fsharplint:disable TypeNames PublicValuesNames MemberNames
namespace SharedUtils

open System.Diagnostics
open System.Collections.Generic
open System
open System.Text.RegularExpressions
open System.IO
open Microsoft.FSharp.Quotations
open System.Xml.Linq

module Assert =
    let AreEqual expected actual = failwith $"Expected: {expected}, Actual: {actual}"

[<AutoOpen>]
module Atomic =
    let now () = DateTime.Now
    let today () = DateTime.Today

    type Is =
        static member GreaterThan a b = b > a

    type Prop() =
        static member inline company_id (objx: ^a) = (^a : (member company_id : int) objx)
        static member inline X (objx: ^a) = (^a : (member X : int) objx)
        //static member inline get_X (objx: ^a) = (^a : (member get_X : int) objx)
        //static member inline set_Y (objx: ^a) = (^a : (member set_Y : (int -> unit)) objx)
        static member inline Y (objx: ^a) = (^a : (member Y : int) objx)
        static member inline id (objx: ^a) = (^a : (member id : _) objx)
        static member inline path (objx: ^a) = (^a : (member path : _) objx)
        static member inline PathX (objx: ^a) = (^a : (member PathX : _) objx)
        static member inline Id (objx: ^a) = (^a : (member Id : _) objx)
        static member inline ConnectionString (objx: ^a) = (^a : (member ConnectionString : _) objx)
        static member inline Project (objx: ^a) = (^a : (member Project : _) objx)
        static member inline UserRoleID (objx: ^a) = (^a : (member UserRoleID : _) objx)
        static member inline week_starting (objx: ^a) = (^a : (member week_starting : _) objx)
        static member inline market_id (objx: ^a) = (^a : (member market_id : int) objx)
        static member inline descr (objx: ^a) = (^a : (member descr : string) objx)
        static member inline Key (objx: ^a) = (^a : (member Key : _) objx)
        static member inline Value (objx: ^a) = (^a : (member Value : _) objx)
        static member inline Name (objx: ^a) = (^a : (member Name : _) objx)
        static member inline Command (objx: ^a) = (^a : (member Command : _) objx)
        static member inline XElement (objx: ^a) = (^a : (member XElement : XElement) objx)
        static member inline Stringify (objx: ^a) = (^a : (member Stringify : unit -> string) objx)

    let Curry2 fn a b = fn(a, b)
    let Curry3 fn a b c = fn(a, b, c)
    let Curry4 fn a b c d = fn(a, b, c, d)
    let Length x = Seq.length x
    let foldx a b c = Seq.fold b a c
    let TotalMilliseconds (t: TimeSpan) = t.TotalMilliseconds |> decimal

    let myNameof (q: Expr<_>) =
        match q with
        | Patterns.Let(_, _, DerivedPatterns.Lambdas(_, Patterns.Call(_, mi, _))) -> mi.Name
        | Patterns.PropertyGet(_, mi, _) -> mi.Name
        | DerivedPatterns.Lambdas(_, Patterns.Call(_, mi, _)) -> mi.Name
        | _ -> failwith "Unexpected format"

    let myNameofx (q: Expr) =
        match q with
        | Patterns.Let(_, _, DerivedPatterns.Lambdas(_, Patterns.Call(_, mi, _))) -> mi.Name
        | Patterns.PropertyGet(_, mi, _) -> mi.Name
        | DerivedPatterns.Lambdas(_, Patterns.Call(_, mi, _)) -> mi.Name
        | Patterns.Call (Some x, y , z) -> y.Name
        | x -> failwith "Unexpected format"

    let (|KVP|) (k: KeyValuePair<'a,'b>) = (k.Key, k.Value)
    [<DebuggerHidden>]
    let str x : string = $"{x}"
    [<DebuggerHidden>]
    let Join (delimiter: string) (items: #seq<string>) = String.Join(delimiter, items)
    let JoinCommaAndSpace (items: #seq<string>) = Join ", " items
    let EmptyFrameworkDictionary() = new Dictionary<_,_>()
    let emptyStringFn() = ""
    //type Appropriateness =
    //    | Certainly     = 1
    //    | Maybe         = 2
    //    | DefinitelyNot = 3
    //type ROPStyle =
    //    | Option = 1
    //    | MS = 2
    //    | Chessie = 3
    //type ROP(s: ROPStyle, x: Appropriateness) =
    //    inherit Attribute()
        //member val xx : Appropriateness = x
    //type ShouldNotUseChessie() = inherit Attribute()
    //type ShouldUseChessie() = inherit Attribute()
    let fst3 (a,b,c) = a
    let TryRepeatedly (attempts: int) (sleepMS: int) fn =
        let rec _fn attempt =
            try
                fn() |> Ok
            with ex ->
                if attempt < attempts then
                    System.Threading.Thread.Sleep sleepMS
                    if attempt > 2 then sprintf "Attempt #%i: %s" attempt ex.Message |> Console.WriteLine
                    _fn <| attempt + 1
                else
                    ex |> Error
        _fn 0

    type FI = FileInfo
    type DI = DirectoryInfo

    let Flatten<'a>(x: 'a option option) =
        match x with
        | Some x -> x
        | _ -> None

    let timed handleTiming fnToRun =
        let start = DateTime.Now
        let result = fnToRun()
        DateTime.Now - start |> handleTiming
        result

    let timedX fnToRun =
        let start = DateTime.Now
        fnToRun(), int (DateTime.Now - start).TotalMilliseconds

    let nullDef dv fn =
        fun arg ->
            if isNull arg then dv
            else fn arg

    let NowMinusTS (offset: TimeSpan) = DateTime.Now.Subtract offset

    let NowMinusDT (offset: DateTime) = DateTime.Now.Subtract offset

    //let TimeToTheMillisecond x = Extensions.ToTimeWithUnits(x, UnitsOfTime.Millisecond)
    //let TimeToTheSecond x = Extensions.ToTimeWithUnits(x, UnitsOfTime.Second)
    //let TimeToTheHour x = Extensions.ToTimeWithUnits(x, UnitsOfTime.Hour)

    //let timedConsole fn = timed (TimeToTheMillisecond >> Console.WriteLine) fn

    //let NewtonSerialize x = UniversalExtensions.Extensions.NewtonSerialize x
    //let NewtonDeserialize x = UniversalExtensions.Extensions.NewtonDeserialize x
    //let Option_SomeToFN (binder: 'a -> 'b option) (opt: 'a option) = Option.bind binder opt

    let Option_Bind (fn: 'a -> 'b option) (maybe: 'a option) : 'b option =
        maybe
        |> function
        | Some x -> fn x |> Some
        | None _ -> None
        |> Flatten
    [<DebuggerHidden>]
    let Option_SomeToFN (fn: 'a -> 'b) (maybe: 'a option) = Option.map fn maybe

    let Nullable_SomeToFN (fn: 'a -> 'b) (maybe: Nullable<'a>) =
        if maybe.HasValue then maybe.Value |> fn |> Some
        else None

    let private cache = new System.Collections.Concurrent.ConcurrentDictionary<string, obj>()
    //let memoize (fn: string -> 'a) paramKey =
    //    match cache.TryGetValue paramKey with
    //    | true, v -> v :?> 'a
    //    | false, _ ->
    //        let v = fn paramKey
    //        cache.TryAdd(paramKey, unbox v) |> ignore
    //        v

    let memoizeNamed (paramParts: string list) paramVal (fn: 'b -> 'a) =
        let joinedKeyName = paramParts |> JoinCommaAndSpace
        match cache.TryGetValue joinedKeyName with
        | true, v -> v :?> 'a
        | false, _ ->
            let v = fn paramVal
            cache.TryAdd(joinedKeyName, unbox v) |> ignore
            v

    let Option_SomeToFNQuiet a b = Option_SomeToFN a b |> ignore

    let FullName = nullDef "" (fun (f: FileInfo) -> f.FullName)
    [<DebuggerHidden>]
    let NoneToX (maybe: 'a option) (defaultValue: 'a): 'a =
        match maybe with
        | Some(x) -> x
        | None -> defaultValue
    let NoneTo (defaultValue: 'a) (maybe: 'a option) : 'a = NoneToX maybe defaultValue
    type hidden = DebuggerHiddenAttribute
    type Existence =
        | Exists
        | NotExists
    let isNotNull x = x |> (isNull >> not)
    let invoke fn = fn ()
    let Label label value = sprintf "%s: %s" label value
    let LabelTuple (label, value) = Label label value
    let makeAction (fn: unit -> unit) : Action = new System.Action(fn)
    let makeAction1 (fn: 'a -> unit) : Action<'a> = new System.Action<'a>(fn)
    let makeFunc (fn: 'a -> 'b) : Func<'a, 'b> = new System.Func<'a, 'b>(fn)
    let makeFuncU (fn: unit -> 'a) : Func<'a> = new System.Func<'a>(fn)
    let groupBy selector items = System.Linq.Enumerable.GroupBy(items, makeFunc selector)
    let LabelObj label value = sprintf "%s: %s" ((sprintf "%A" label).Trim(char @"""")) ((sprintf "%A" value).Trim(char @""""))
    let LabelInt label value = sprintf "%s: %i" label value
    let returner item () = item
    let ret item () = item
    let retInstead x y = x
    let retFn (fn: 'a -> 'b) : 'a -> unit -> 'b = (fun x () -> fn x)
    let retX () item = item
    [<DebuggerHidden>]
    let flipParams2 (fn: 'a -> 'b -> 'c) = (fun (b: 'b) (a: 'a) -> fn a b)
    [<DebuggerHidden>]
    let flip = flipParams2
    let len x = Seq.length x
    let getType x = x.GetType()
    let getTypeName x = (x |> getType).Name
    let JoinLines(text: #seq<string>): string = String.Join("\r\n", text |> Seq.toArray)
    let Append (stringToAppend: string) something = something + stringToAppend
    let AppendNewLine = Append "\r\n"
    let ToString(obj: obj) =
        if isNull obj then ""
        else obj.ToString()
    let JoinPeriodSpace txt = Join ". " txt

    [<DebuggerHidden>]
    let (|AsString|) (obj: 'a) = str obj
    let Replace (AsString needle: string) (AsString replacement: string) (AsString haystack: string) = haystack.Replace(needle, replacement)
    let IsNullOrEmpty x = String.IsNullOrEmpty x
    let IsNotEmpty =  IsNullOrEmpty >> not
    let foldi fold first source =
        source
        |> Seq.fold (fun (prev, i) c -> (fold i prev c, i + 1)) (first, 0)
        |> fst
    let ns (x: obj) : string = if isNull x then "" else $"{x}"
    let (|NS|) (x: string) : string = x |> ns
    let ToUpper(NS str) = str.ToUpper()
    let ToLower(NS str) = str.ToLower()

    let OPns = Option.defaultValue ""
    let lower x = (x |> ns).ToLower()
    let upper x = (x |> ns).ToUpper()
    let Trim (x: string) = x |> ns |> fun x -> x.Trim()
    let ToList x = Seq.toList x
    [<DebuggerHidden>]
    let RegexReplace (pattern: string) (replacement: string) (input: string) : string =
        let input = input |> ns
        let pattern = pattern |> ns
        let replacement = replacement |> ns
        Regex.Replace(input, pattern, replacement, (RegexOptions.IgnoreCase ||| RegexOptions.Singleline))
    [<DebuggerHidden>]
    let RegexStrip (pattern: string) (input: string) = input |> RegexReplace pattern ""
    let SingleOrNone x =
        x
        |> ToList
        |> function
        | [x] -> Some x
        | x -> None

    let FirstOrNone(items: 'a seq): 'a option =
        items
        |> Seq.toList
        |> function
        | [] -> None
        | firstEl :: x -> Some firstEl

    let FirstOrDefault (defaultValue: 'a) (items: 'a seq): 'a =
        items
        |> FirstOrNone
        |> (NoneTo defaultValue)

    let SomeIf predicate value =
        if predicate then Some(value)
        else None
    let EmptyToDefault (defaultValue: string) (source: string) =
        if source |> IsNullOrEmpty then defaultValue
        else source

    [<hidden>]
    let UnwrapSome =
        function
        | Some (Some x) -> Some x
        | _ -> None

    [<hidden>]
    let UnwrapSomes<'a>(somes: seq<'a option>) =
        somes
        |> Seq.where (fun oo -> oo.IsSome)
        |> Seq.map (fun oo -> oo.Value)
        |> Seq.toList

    let TrimStart(startText: string) : string -> string = RegexStrip("^" + Regex.Escape(startText |> ns))

    type RegexBuilderElement =
        | Input of string
        | Pattern of string
        | Replacement of string
        | RegexOptions of RegexOptions

    type RegexBuilder =
        { Input: string option
          Pattern: string option
          Replacement: string option
          ReplacementCount: int option
          RegexOptions: RegexOptions
          MatchEvaluator: MatchEvaluator option }
        static member Empty =
            { Input = None
              Replacement = None
              Pattern = None
              ReplacementCount = None
              RegexOptions = RegexOptions.IgnoreCase
              MatchEvaluator = None }

    type RX =
        static member MultiLine rb = { rb with RegexOptions = rb.RegexOptions ||| RegexOptions.Multiline }
        static member ExplicitCapture rb = { rb with RegexOptions = rb.RegexOptions ||| RegexOptions.ExplicitCapture }

        static member Init (elements: RegexBuilderElement list) =
            let foldRx state item =
                match item with
                | Input x -> state |> RX.Input x
                | Pattern x -> state |> RX.Pattern x
                | Replacement x -> state |> RX.Replacement x
                | RegexOptions x -> state |> RX.Options x
            elements
            |> ToList
            |> List.fold foldRx RegexBuilder.Empty

        static member private InvalidArgs() = failwith "Invalid arguments for RegexBuilder"

        [<hidden>]
        static member Empty = RegexBuilder.Empty

        [<hidden>]
        static member Input text rb = { rb with Input = Some text }

        //[<hidden>]
        static member RegexPad (rb: RegexBuilder) = RX.RegexPadFull rb |> ignore

        static member RegexPadFull (rb: RegexBuilder) =
            let ns =
                function
                | Some x -> x
                | _ -> ""
            File.WriteAllText(@"c:\temp\regexpad.input.txt", rb.Input |> ns)
            File.WriteAllText(@"c:\temp\regexpad.pattern.txt", rb.Pattern |> ns)
            File.WriteAllText(@"c:\temp\regexpad.replacement.txt", rb.Replacement |> ns)
            File.WriteAllText(@"c:\temp\regexpad.options.txt", rb.RegexOptions |> int |> str)
            Process.Start(@"C:\DEV\Releases\RegexPad\RegexPad.exe", "import")

        static member RegexPadWait (rb: RegexBuilder) : unit = (RX.RegexPadFull rb).WaitForExit()

        [<hidden>]
        static member Pattern text (rb: RegexBuilder) = { rb with Pattern = Some text }

        [<hidden>]
        static member StartWithPattern x = RX.Pattern x RX.Empty

        [<hidden>]
        static member StartWithInput x = RX.Input x RX.Empty

        [<hidden>]
        static member FromInput = RX.StartWithInput

        [<hidden>]
        static member FromPattern = RX.StartWithPattern

        [<hidden>]
        static member Replacement text rb = { rb with Replacement = Some text }

        [<hidden>]
        static member ReplacementCount text rb = { rb with ReplacementCount = Some text }

        [<hidden>]
        static member ReplacementFn (rep: Match -> string) rb =
            { rb with MatchEvaluator = new System.Text.RegularExpressions.MatchEvaluator(rep) |> Some }

        static member Options opts rb = { rb with RegexOptions = opts }

        [<hidden>]
        static member IsMatch =
            function
            | { Input = Some(i); Pattern = Some(p); Replacement = None; RegexOptions = o } -> Regex.IsMatch(i, p, o)
            | _ -> RX.InvalidArgs()

        [<hidden>]
        static member IsMatchOrContains =
            function
            | { Input = Some(i); Pattern = Some(p); Replacement = None; RegexOptions = o } ->
                try Regex.IsMatch(i, p, o)
                with ex -> i.ToUpper().Contains(p.ToUpper())
            | _ -> RX.InvalidArgs()

        static member GroupNames (rb: RegexBuilder) =
            rb
            |> function
            | { Pattern = Some(p) } ->
                let r = new Regex(p)
                r.GetGroupNames()
                |> ToList
            | _ -> []

        static member MatchesDicts (rb: RegexBuilder) : IDictionary<string, string> list =
            let names =
                rb
                |> RX.GroupNames
            rb
            |> RX.Matches
            |+ fun x ->
                
                names
                |+ fun q -> q, x.Groups.[q].Value
                |> dict
            |> ToList

        static member Matches (rb: RegexBuilder) : Match list =
            rb
            |> function
            | { Input = Some(i); Pattern = Some(p); Replacement = None; RegexOptions = o } ->
                Regex.Matches(i, p, o)
                |> Seq.cast<Match>
                |> ToList
            | _ -> RX.InvalidArgs()

        static member FirstGroupMatches =
            let firstGroupValue(m: Match) = m.Groups.[1].Value
            RX.Matches
            >> (Seq.map firstGroupValue)
            >> ToList

        static member Create =
            function
            | { Pattern = Some pattern; RegexOptions = ro } -> new Regex(pattern, ro)
            | _ -> RX.InvalidArgs()

        static member Replace =
            function
            | { Input = Some(i); Pattern = Some(p); Replacement = Some(r); RegexOptions = o; ReplacementCount = None } -> Regex.Replace(i, p, r, o)
            | { Input = Some(i); Pattern = Some(p); Replacement = Some(r); RegexOptions = o; ReplacementCount = Some rc } ->
                let mutable hit = 0
                let me = new MatchEvaluator(fun m ->
                    hit <- hit + 1
                    if hit > rc then m.Value
                    else r)
                Regex.Replace(i, p, me, o)
            //| { Input = Some(i); Pattern = Some(p); Replacement = Some(r); RegexOptions = o } -> Regex.Replace(i, p, r, o)
            | { Input = Some(i); Pattern = Some(p); MatchEvaluator = Some(me); RegexOptions = o; ReplacementCount = None } -> Regex.Replace(i, p, me)
            | x -> RX.InvalidArgs()

        static member Strip =
            function
            | { Input = Some(i); Pattern = Some(p); Replacement = None; RegexOptions = o } -> Regex.Replace(i, p, "", o)
            | _ -> RX.InvalidArgs()

        static member Match =
            function
            | { Input = Some(i); Pattern = Some(p); Replacement = None; RegexOptions = o } -> Regex.Match(i, p, o)
            | _ -> RX.InvalidArgs()

        static member Split : RegexBuilder -> string list =
            function
            | { Input = Some(i); Pattern = Some(p); Replacement = None; RegexOptions = o } -> Regex.Split(i, p, o)
            | _ -> RX.InvalidArgs()
            >> Seq.toList

        static member Escape = Regex.Escape
        static member Unescape = Regex.Unescape
    [<DebuggerHidden>]
    let Lines (text: string) = System.Text.RegularExpressions.Regex.Split(text |> ns, @"\r\n|\n\r|\n|\r")

    type ShouldIncludeStackTrace =
        | IncludeStackTrace
        | DoNotIncludeStackTrace

    type ShouldRecurse =
        | Recurse
        | DoNotRecurse

    let EmptyTo dv (x: string) =
        if String.IsNullOrEmpty x then dv
        else x

    let AppendIfNotEmpty (whatToAppend: string) (stringVal: string) =
        if stringVal |> Extensions.u_IsEmpty then ""
        else stringVal + whatToAppend

    type SpecificTypeHandler = (exn -> string) -> exn -> string option

    let rec TheNewBestExceptionToString (specificTypeHandlers: SpecificTypeHandler list) includeStackTrace recursive (ex: exn) =
        let rec work indentLevel (ex: exn) =
            let nextIndentLevel = indentLevel + 1
            let work = work nextIndentLevel
            let exnType = $"Exception: {ex.GetType().Name}"
            let msg = $"Message: {ex.Message}"
            let prependIndentation str =
                let indent = new string(' ', 4 * indentLevel)
                str
                |> Lines
                |> Array.map (fun x -> (*$"{indentLevel}" +*) indent + x)
                |> JoinLines

            let stackTrace =
                function
                | IncludeStackTrace -> ex.StackTrace
                | DoNotIncludeStackTrace -> ""

            let aggHandle (ex: AggregateException) =
                ex.InnerExceptions
                |> Seq.cast<exn>
                |> Seq.map work
                |> JoinLines

            let recursion =
                function
                | Recurse ->
                    match ex with
                    | :? AggregateException as ex -> aggHandle ex
                    | ex ->
                        match (ex.InnerException) with
                        | null -> ""
                        | iex -> work iex
                | _ -> ""

            let internalSpecificTypeHandlers =
                match ex with
                | :? System.Reflection.ReflectionTypeLoadException as x -> x.LoaderExceptions |+ TheNewBestExceptionToString specificTypeHandlers IncludeStackTrace Recurse |> JoinLines
                //| :? System.Data.Entity.Core.EntityClient.EntityCommandExecutionException  as x -> x.LoaderExceptions |+ TheNewBestExceptionToString IncludeStackTrace Recurse |> JoinLines
                | _ -> ""

            specificTypeHandlers
            |+ fun handler -> handler work ex
            |> UnwrapSomes
            |> List.filter (String.IsNullOrWhiteSpace >> not)
            |>| function
            | [] ->
                [
                    exnType |> prependIndentation
                    msg |> prependIndentation
                    stackTrace includeStackTrace |> prependIndentation
                    recursion recursive
                    internalSpecificTypeHandlers
                ]
            | x -> x
            |> List.filter (String.IsNullOrWhiteSpace >> not)
            |> List.map ns
            |> JoinLines
            |> AppendIfNotEmpty "\r\n"
        if isNull ex then ""
        else (work 0 ex).TrimEnd()

    let TheNewBestExceptionToStringFull ex = TheNewBestExceptionToString [] IncludeStackTrace Recurse ex
    let MapGrid (fn: 'item -> 'result) (rows: 'item list list) = rows |+| (List.map fn)

    type Whitespace =
        | NoneX
        | TrimLines
        | RemoveAllWhitespace

    let NormalizeLineEndings (valx: string) (newLine: string): string =
        let nl : string =
            if newLine = "" then Environment.NewLine
            else newLine
        Regex.Replace(valx, @"\r\n|\n\r|\n|\r", nl)

    let FirstValidPathX paths =
        paths
        |?| fun x -> File.Exists x
        |> function
        | [] -> None
        | x :: _ -> Some x

    let WinMerge v1 v2 leftPostFix rightPostFix =
        let mutable v1 = NormalizeLineEndings v1 "\r\n"
        let mutable v2 = NormalizeLineEndings v2 "\r\n"
        v1 <- v1.Trim()
        v2 <- v2.Trim()

        let IsSomething(obj : obj) : bool =
            obj
            |> function
            | :? string as x -> String.IsNullOrWhiteSpace x |> not
            | x -> x = null
        let mutable leftPostFix = leftPostFix
        let mutable rightPostFix = rightPostFix

        if leftPostFix  |> IsSomething then leftPostFix  <- "-" + leftPostFix
        if rightPostFix |> IsSomething then rightPostFix <- "-" + rightPostFix
        let left = $@"c:\temp\Left{leftPostFix}"
        let right = $@"c:\temp\Right{rightPostFix}"
        File.WriteAllText(left, v1)
        File.WriteAllText(right, v2)
        if v1 = v2 then true
        else
            [
                @"C:\Sync\PortableApps\WinMergePortable\WinMergePortable.exe"
                @"C:\Program Files (x86)\WinMerge\WinMergeU.exe"
                @"c:\dev\PAUL\WinMerge\WinMergeU.exe"
                @"D:\Program Files (x86)\WinMerge\WinMergeU.exe"
            ]
            |> FirstValidPathX
            |> function
            | Some wm -> Process.Start(wm, $"{left} {right}") |> ignore
            | _ -> ()
            false

    let AreEqualWinMerge (expected) actual (trimLinesFirst: Whitespace) expectedHeader actualHeader =
        //if TrimLinesFirst = -1 then
        //    failwith "Failballs 55"

        let trimLines (str : string) : string =
            str |> Lines |+ Trim |> JoinLines
            //(From l In str.Lines Select l.Trim).strJoin(vbNewLine)
        //let mutable expected = NormalizeLineEndings expected "\r\n"
        //let mutable actual =   NormalizeLineEndings actual   "\r\n"

        let excludeContextLines (code: string): string =
            code
            |> Lines
            |?! fun x -> Regex.IsMatch(x, "__context:=")
            |> JoinLines

        let fn =
            flip NormalizeLineEndings "\r\n"
            >> match trimLinesFirst with
                | Whitespace.TrimLines ->
                    trimLines
                | Whitespace.RemoveAllWhitespace ->
                    fun x ->
                        let re = new Regex("[ \t]")
                        re.Replace(x, "")

                | _ -> id
            >> Trim
            >> excludeContextLines

        try
            Assert.AreEqual expected actual
        with ex ->
            try
                Assert.AreEqual (fn expected) (fn actual)
            with ex ->
                let vbNewLine = "\r\n"
                let caller = ""
                if WinMerge (expectedHeader + vbNewLine + expected) (actualHeader + vbNewLine + actual) caller caller |> not then reraise()

[<AutoOpen>]
module Atomic2 =
#if FRAMEWORK
    let regexReplaceCaseSensitive pat rep inp =
        RX.StartWithPattern pat
        |> RX.Replacement rep
        |> RX.Input inp
        |> RX.Options RegexOptions.None
        |> RX.Replace

    let SpacifyName x =
        let acronyms = [
            "DLL"
            "XML"
            "CSV"
            "PDB"
        ]
        let spacifyAcronyms (start: string) = Seq.fold (fun (state: string) item -> state.Replace(item, $" {item} ")) (str start) acronyms
        x
        |> spacifyAcronyms
        |> (ns
        >> ToString
        >> Replace "__" "~~"
        >> Replace "_" " "
        >> (regexReplaceCaseSensitive "([a-z])([A-Z0-9])" "$1 $2")
        >> (regexReplaceCaseSensitive "([^~([{])([A-Z][a-z])" "$1 $2")
        >> Replace "  " " "
        >> Replace "~~" "")
#else
    let regexReplaceCaseSensitive pat rep (inp: string) = inp

    [<DebuggerHidden>]
    let SpacifyName (x: string) = x
#endif
