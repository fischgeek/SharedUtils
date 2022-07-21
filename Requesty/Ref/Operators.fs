namespace SharedFSharpVanilla
open System.IO
//open SharedFSharpVanilla.CoreUtils
open System
open System.Diagnostics
open System.Text.RegularExpressions
//open ExceptionalCode

[<AutoOpen>]
module ExperimentalOperators =
    let inline (^<|) f a = f a
    let inline (^) f a = f a
    let (>>>--) a b = a >> Tee b
    let (>>>|--) a b = a >>>-- (ignore >> b)
    let (>>>--|) a b = a >>>-- b >> ignore
    [<DebuggerHidden>]
    let (</>) (a: 'a option) (b: 'a -> 'b) = a |> Atomic.Option_SomeToFN b
    //[<Obsolete("Switch to </>", false)>]
    let (<+>) a b  =
        a
        |> Option.map b
        |> Atomic.UnwrapSome
    let (<%>) (a: 'a option) b = a </> Seq.iter b |> ignore
    let (<=>) (a: 'a option) (b: 'a -> 'b option) : 'b option = a |> Atomic.Option_Bind b
    let (<+|>) (a: 'a option) (b: 'a -> unit) : unit = a |> Option_SomeToFN b |> ignore
    let private dorp (incoming: 'inType -> 'outType option) (fn: 'outType -> unit)  =
        fun x ->
            x
            |> incoming
            |> Option.map fn
            |> ignore
    let (>><+|>) incoming fn = dorp incoming fn
    let (<!>) (a: 'a option) (b: 'a) = a |> Option.defaultValue b
    let (<++>) (a: Nullable<'a>) (b: 'a -> 'b) = a |> Nullable_SomeToFN b
    let OptionFilter (optionalItems: #seq<'a option>) (predicate: 'a -> bool) =
        optionalItems
        |? fun x ->
            x
            |> function
            | Some x -> x |> predicate
            | None -> false
        |+ fun x -> x.Value
    let OptionFilterX a b = OptionFilter b a
    let (<?>) (optionalItems: #seq<'a option>) (predicate: 'a -> bool) = OptionFilter optionalItems predicate
    let (<!+>) (a: 'a option) (b: unit -> 'a) = a |> OptionPipe.NoneToFN b
    let (>><+>) maybe (fn: 'a -> 'b option) = maybe >> Option_SomeToFN fn
    let (>></>) maybe (fn: 'a -> 'b) = maybe >> Option_SomeToFN fn
    let bindOptionToMaybe (fn: 'input -> 'ret option) (defVal: 'ret) : 'input -> 'ret =
        fn
        >> fun x ->
            match x with
            | Some x -> x
            | None -> defVal
    let (>><!>) maybe (fn: 'a -> 'b) = maybe >> OptionPipe.NoneTo fn
    let (<-->) (a: 'a option) (b: 'a -> unit) = a |> Option_SomeToFN (Tee b)

    let (>><|>) (a: 'itemIn -> 'itemOut option) (b: 'itemOut -> _) : 'itemIn -> unit = a >> (Option_SomeToFNQuiet b)
    let (<|>) (a: 'a option) (b: 'a -> 'b) : unit =
        a
        |> Option_SomeToFNQuiet b

    let (|--|) a b = a |-- (b >> ignore)
    //let (<||>) a b =
    //    a
    //    <|> (b >> ignore)
    //let twoParam a b = a + b
    //let threeParam a b c = c + a + b |> ignore
    //let z x = ()
    //twoParam 1 2 |> z
    //let fnx2 = twoParam >>> z
    //let fnx3 = threeParam >>>> z

    //let fn1 (arg1: string) (arg2: int) : DateTime = DateTime.Now
    //let fn2 (arg1: DateTime) = 55
    //let composedWithOperator = fn1 >>> fn2

[<AutoOpen>]
module Operators =
    // fsharplint:disable-next-line PublicValuesNames
    let id_ x () = x

    [<DebuggerHidden>]
    let Tee fnct obj =
        fnct obj
        obj
    let private composeWithTwoParameters (fnLeft: 'arg1 -> 'arg2 -> 'middleType) (fnRight: 'middleType -> 'c) (arg1: 'arg1) (arg2: 'arg2) =
        fnLeft arg1 arg2 |> fnRight
    //let (>>>) a b = composeWithTwoParameters a b
    let (>>>) a b = a >>> b
    let private composeWithTwoParametersSwap (fnLeft: 'arg1 -> 'arg2 -> 'middleType) (fnRight: 'middleType -> 'c) (arg2: 'arg2)
        (arg1: 'arg1) = fnLeft arg1 arg2 |> fnRight
    let (><>) a b = composeWithTwoParametersSwap a b
    let private composeWithThreeParameters (fnLeft: 'arg1 -> 'arg2 -> 'arg3 -> 'middleType) (fnRight: 'middleType -> 'c) (arg1: 'arg1)
        (arg2: 'arg2) (arg3: 'arg3) = fnLeft arg1 arg2 arg3 |> fnRight
    let (>>>>) a b = composeWithThreeParameters a b

    let (<||||) fn args =
        let (a, b, c, d) = args
        fn a b c d

    [<DebuggerHidden>]
    let ToList = Seq.toList

    [<DebuggerHidden>]
    let ToObj x = x :> obj

    [<DebuggerHidden>]
    let ToArray = Seq.toArray

    [<DebuggerHidden>]
    let ToSeq = List.toSeq
    #if FRAMEWORK
    [<DebuggerHidden>]
    let ToArrayList x = new System.Collections.ArrayList(x |> ToArray)
    #endif
    [<DebuggerHidden>]
    let Concat = String.concat
#if FRAMEWORK
    let (+/+) (x: string) (y: string) : string = Path.Combine(x, y)
    let cleanPath (x: string) =
        x
        |> Atomic.RegexReplace @"/" @"\"

    let (.//) (dir: DirectoryInfo) (path: string) : DirectoryInfo option =
        Path.Combine(dir.FullName, path |> cleanPath)
        |> Constructors.DirectoryInfo
    let (./) (dir: DirectoryInfo) (path: string) : string = Path.Combine(dir.FullName, path |> cleanPath)
    let (./.) (dir: DirectoryInfo) (path: string) : FileInfo = Path.Combine(dir.FullName, path |> cleanPath) |> FI
    let (./+) (dir: DirectoryInfo) (pathChunks: string list) : string = Path.Combine([dir.FullName] @ pathChunks |+ cleanPath |> ToArray)
    let (./*) (dir: DirectoryInfo) (pattern: string) : FI list =
        if dir.Exists then
            dir.GetFiles(pattern)
            |> ToList
        else []

    let (./?) (dir: DirectoryInfo) (fileSearchPattern: string) : FileInfo list =
        dir
        |> function
        | null -> []
        | x -> x.GetFiles(fileSearchPattern) |> Array.toList
#endif
    let (@@) (item: 'a) (items: 'a seq) = [ item ] @ (items |> ToList)
    let (@>>) (item: 'a) (items: 'a seq) = [ item ] @ (items |> ToList)
    let (<<@) (items: 'a seq) (item: 'a) = (items |> ToList) @ [ item ]
    let (@@>>) (itemsStart: 'a seq) (itemsToAppend: 'a seq) = itemsToAppend |> Seq.append itemsStart
    let (<<@@) = (@@>>)
    let (&->) (item: 'a) (items: 'a seq) = (items |> ToList) @ [ item ] //Append x &-> [a,b,c] = [a,b,c,x]
    let (<-&) (items: 'a seq) (item: 'a) = [ item ] @ (items |> ToList) //Append [a,b,c] &-> x = [x,a,b,c]
    let (<&) = (<<@)
    //let (>>=?!) a b= fail
    let (&>) = (@>>)

    [<DebuggerHidden>]
    let (|%) a b = Seq.iter b a

    [<DebuggerHidden>]
    let (|%%) a b = Seq.iteri b a

    [<hidden>]
    let (>>%) leftFn rightFn = leftFn >> (Seq.iter rightFn)

    let (>>+) leftFn rightFn = leftFn >> (Seq.map rightFn)
    //let (>>>+) leftFn rightFn = leftFn >>> (Seq.map rightFn)

    let (>>-) leftFn rightFn =
        leftFn
        >> rightFn
        >> Seq.concat

    let (>>+-) leftFn rightFn = leftFn >>+ rightFn >> Seq.concat
    let (>>+~) leftFn rightFn = leftFn >>+ rightFn >> UnwrapSomes
    let (>>~) leftFn rightFn = leftFn >> (OptionPipe.SomeToFN rightFn)

    let (>>~+) leftFn rightFn =
        leftFn
        >> (OptionPipe.SomeToFN(Seq.map rightFn))
        >> OptionPipe.NoneTo Seq.empty
        >> ToList

    [<DebuggerHidden>]
    let (|/) = MathPipe.DivideIntByInt
    //>> (OptionPipe.NoneTo 0 )

    let (<-<) txt label = Atomic.Label label txt
    let (<--<) txt label = txt |> str |> Atomic.Label (str label)
    let (>->) label txt = Atomic.Label label txt
    let (>-->) label obj = Atomic.Label (str label) (str obj)

    let (<<-<) obj label = Atomic.LabelObj label obj
    let (>->>) label obj = Atomic.LabelObj label obj
    //let (<-<<) a b = Atomic.LabelNoChar a b

    [<DebuggerHidden>]
    let (|+) a b = Seq.map b a

    let (|++) items fn =
        items
        |> Seq.mapi fn

    let (|++|) items fn =
        items
        |++ fn
        |> ToList

    let (|+|) a b = Seq.map b a |> ToList
    let (|+||) a b = Seq.map b a |> ToArray

    [<DebuggerHidden>]
    let (|>|) a b =
        a
        |> Seq.toList
        |> b

    [<DebuggerHidden>]
    let (|-+) a b = a |> Seq.append b

    let (|-++) a b =
        a
        |> Seq.concat
        |> b

    let (|~/) a b = Regex.Split(a, b, RegexOptions.IgnoreCase)

    let (|>||) a b =
        a
        |> Seq.toArray
        |> b

    [<DebuggerHidden>]
    let TeeIterX obj fnct =
        obj |> Seq.iter fnct
        obj

    let TeeIter fnct obj = TeeIterX obj fnct
    let (|--%) = TeeIterX

    [<DebuggerHidden>]
    let (|--) obj fnct = Tee fnct obj
    //[<DebuggerHidden>]
    //let (|--) obj fnct =
    //    fnct obj
    //    obj

    [<DebuggerHidden>]
    let (|?) items predicate = items |> Seq.filter predicate

    let (|.) items fn = items |+ (fun x -> (x, fn x))
    let (|.?) (items: ('a * 'b) seq) (test: 'b -> bool) = items |? (fun (_, prop) -> test prop) |+ fst
    let (|.?=) (items: ('a * 'b) seq) (test: 'b) = items |? (fun (_, prop) -> test = prop) |+ fst
    let (|.?!) (items: ('a * 'b) seq) (test: 'b -> bool) = items |.? (test >> not)

    //[<DebuggerHidden>]
    //let (|??) items (predicate, projector) = items |> Seq.filter (fun x-> x |> projector |> predicate) //probably pointless

    [<DebuggerHidden>]
    let (|?|) items predicate = items |? predicate |> Seq.toList

    [<DebuggerHidden>]
    let (|?!) items predicate = items |? (not << predicate )

    //[<DebuggerHidden>]
    //let (>>?) (itemsFn: 'a -> #seq<'b>) (predicate: 'b -> bool) = ExceptionalCode.Operators.composeWhere itemsFn predicate
    //let (>>?) (itemsFn: 'a -> #seq<'b>) (predicate: 'b -> bool) = ExceptionalCode.Operators.composeWhere itemsFn predicate

    //[<DebuggerHidden>]
    //let (>>|?) itemsFn predicate = XX.bb itemsFn predicate

    [<DebuggerHidden>]
    let (>>?!) leftFn rightFn =
        leftFn
        >> (Seq.where (rightFn >> not))

    [<DebuggerHidden>]
    let (|?=) items target = items |? (fun x -> x = target)

    [<DebuggerHidden>]
    let (|?!=) items target = items |? (fun x -> x <> target)

    [<DebuggerHidden>]
    let (|?!|) items predicate = items |?! predicate |> Seq.toList

    [<DebuggerHidden>]
    let (|?!||) items predicate = items |?! predicate |> Seq.toArray

    [<DebuggerHidden>]
    let (.*?) (input: string) (pattern: string) = Regex.IsMatch(input, pattern, RegexOptions.IgnoreCase)

    [<DebuggerHidden>]
    let (.*=) (input: string) (pattern: string) = Regex.Matches(input, pattern, RegexOptions.IgnoreCase)

    [<DebuggerHidden>]
    let (.*>) (input: string) (pattern: string) (replacement: string) = Regex.Replace(input, pattern, replacement, RegexOptions.IgnoreCase)

    [<DebuggerHidden>]
    let (.*/) (input: string) (pattern: string) = Regex.Split(input, pattern, RegexOptions.IgnoreCase)

    [<DebuggerHidden>]
    let (>>|+) a b =
        (fun z ->
        z
        |> a
        |+ b)

    [<DebuggerHidden>]
    let (>>|+|) a b = a >>|+ b >> ToList

    [<DebuggerHidden>]
    let (>>|+||) a b = a >>|+ b >> ToArray

    [<DebuggerHidden>]
    let (>>|%) a b =
        (fun z ->
        z
        |> a
        |% b)
