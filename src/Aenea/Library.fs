namespace Aenea

open FsCheck

module Model =
    type State =
        | Normal
        | Focused
        | Pending

    type ExecutionType =
        | Parallel
        | Seqence

    type AeneaTest =
        | TestGroup of label: string * tests: AeneaTest list * config : (Config -> Config) option * state: State
        | TestCase of label: string * testCode: (Config -> unit) * config: (Config -> Config) option * state: State

    type TestExecution = {
        label: string
        code: Config -> unit
        configMapper: (Config -> Config) option
        state: State
    }


    type ExecutionModel =
        | ETestCase of TestExecution
        | ETestGroup of ExecutionModel list * ExecutionType

[<AutoOpen>]
module Core =
    open Model

    let getExecutionModel (test: AeneaTest) : ExecutionModel =
        let rec helper (label: string) (config: (Config -> Config) option) (parentState: State) (t: AeneaTest) : ExecutionModel =
            match t with
            | TestCase(name, code, cfg, state) ->
                let newState =
                    match state, parentState with
                    | Normal, _ -> parentState
                    | _, _ -> state
                let c =
                    match config, cfg with
                    | Some pc, Some c -> Some (pc >> c)
                    | Some c, None -> Some c
                    | None, Some c -> Some c
                    | _ -> None
                let o = {label = label + "/" + name; code = code; configMapper = c; state = newState}
                ETestCase o
            | TestGroup(name, tests, cfg, state) ->
                let newState =
                    match state, parentState with
                    | Normal, _ -> parentState
                    | _, _ -> state
                let c =
                    match config, cfg with
                    | Some pc, Some c -> Some (pc >> c)
                    | Some c, None -> Some c
                    | None, Some c -> Some c
                    | _ -> None
                let tests = tests |> List.map (helper (label + "/" + name) c newState)
                ETestGroup (tests, Parallel)
        helper "" None Normal test

    let rec flattenExecModel = function
        | ETestCase ec -> List.singleton ec
        | ETestGroup (tests,_) ->
            tests
            |> List.collect flattenExecModel

    let rec filterExecModel prop = function
        | ETestCase ec when prop ec -> Some (ETestCase ec)
        | ETestCase ec when not (prop ec) -> None
        | ETestGroup (tests,et) ->
            let tests =
                tests
                |> List.choose (filterExecModel prop)
            Some <| ETestGroup(tests, et)

    let rec runExecutionModel = function
        | ETestCase x ->
            if x.state = Pending then
                ()
            else
                let c =
                    match x.configMapper with
                    | Some apply -> apply Config.Default
                    | None -> Config.Default
                let c = {c with Name = x.label}
                // printfn "Starting %s" name
                x.code c
                // printfn "Finished %s" name
        | ETestGroup(tests, Parallel) ->
            tests
            |> List.toArray
            |> Array.Parallel.iter runExecutionModel
        | ETestGroup(tests, Seqence) ->
            tests
            |> List.iter runExecutionModel

    let run (test: AeneaTest) =
        let exuctionModel = getExecutionModel test
        let isAnyFocused =
            exuctionModel
            |> flattenExecModel
            |> List.exists (fun x -> x.state = Focused)
        let execution =
            if isAnyFocused then
                exuctionModel |> filterExecModel (fun x -> x.state = Focused)
            else
                Some exuctionModel
        match execution with
        | None -> ()
        | Some exuction ->
            runExecutionModel exuction

    let withConfig (configTransform: Config -> Config) (t: AeneaTest) =
        match t with
        | TestGroup (l, t, oldConfig, state) ->
            let newConfig =
                match oldConfig with
                | Some c -> c >> configTransform
                | None -> configTransform
            TestGroup(l,t,Some newConfig, state)
        | TestCase(label, testCode, oldConfig, state) ->
            let newConfig =
                match oldConfig with
                | Some c -> c >> configTransform
                | None -> configTransform


            TestCase(label, testCode, Some newConfig, state)



module ListDSL =
    open Model

    let testGroup name lst = TestGroup(name, lst, None, Normal)
    let test name (f: 'Testable) =
        let quickCheckTest cfg = Check.One(cfg, f)
        TestCase(name, quickCheckTest, None, Normal)
    let example name (f: 'Testable) =
        let quickCheckTest cfg = Check.One({cfg with MaxTest = 1}, f)
        TestCase(name, quickCheckTest, None, Normal)

    let ptestGroup name lst = TestGroup(name, lst, None, Pending)
    let ptest name (f: 'Testable) =
        let quickCheckTest cfg = Check.One(cfg, f)
        TestCase(name, quickCheckTest, None, Pending)

    let pexample name (f: 'Testable) =
        let quickCheckTest cfg = Check.One({cfg with MaxTest = 1}, f)
        TestCase(name, quickCheckTest, None, Pending)

    let ftestGroup name lst = TestGroup(name, lst, None, Focused)
    let ftest name (f: 'Testable) =
        let quickCheckTest cfg = Check.One(cfg, f)
        TestCase(name, quickCheckTest, None, Focused)

    let fexample name (f: 'Testable) =
        let quickCheckTest cfg = Check.One({cfg with MaxTest = 1}, f)
        TestCase(name, quickCheckTest, None, Focused)




module CeDSL =
    open Model
    // test "" {
    //     focused //pending
    //     max_test 12
    //     quiet_on_success
    //     fun (xs:list<int>) -> List.rev(List.rev xs) = xs
    // }

    type TestCaseState<'a> = {
        Name: string
        State: State
        TestCode: Property option
        IsExample: bool
        WithConfig: (Config -> Config) option
    }

    type TestCaseBuilder<'a> (name: string) =
        let mutable state : TestCaseState<'a> = {Name = name; State = Normal; TestCode = None; IsExample = false; WithConfig = None}

        member __.Zero () =
            state

        [<CustomOperation("max_test")>]
        member x.MaxTest(st, value) =
            let apply = fun cfg -> {cfg with MaxTest = value}
            x.WithConfig(st, apply)

        [<CustomOperation("quiet_on_success")>]
        member x.QuietOnSuccess(st, value) =
            let apply = fun cfg -> {cfg with QuietOnSuccess = value}
            x.WithConfig(st, apply)

        [<CustomOperation("with_config")>]
        member __.WithConfig(st, (value: Config -> Config)) =
            let newWithConfig =
                match state.WithConfig with
                | Some wc -> wc >> value
                | None -> value

            state <- {st with WithConfig = Some newWithConfig}
            state

        [<CustomOperation("focused")>]
        member __.Focused(st) =
            state <- {st with State = Focused}
            state

        [<CustomOperation("pending")>]
        member __.Pending(st) =
            state <- {st with State = Pending}
            state

        [<CustomOperation("example")>]
        member __.IsExample(st) =
            state <- {st with IsExample = true}
            state

        member __.Yield(other: unit) =
            state

        member __.Yield(code: 'a -> Property) =
            state <- {state with TestCode = Some (Prop.ofTestable code)}
            state

        member __.Yield(code: 'a -> bool) =
            state <- {state with TestCode = Some (Prop.ofTestable code)}
            state

        member __.Delay f = f()

        member __.Combine(st: TestCaseState<'a>, ns: TestCaseState<'a>) = state

        member __.For(st, func) =
            __.Delay(fun () -> func ())

        member __.Run (state: TestCaseState<'a>) =
            let quickCheckTest =
                match state.TestCode with
                | Some tc ->
                    if state.IsExample then
                        fun cfg ->
                            Check.One({cfg with MaxTest = 1}, tc)
                    else
                        fun cfg ->
                            Check.One(cfg, tc)
                | None -> failwith "Some test code needs to be returned"

            TestCase(state.Name, quickCheckTest, state.WithConfig, state.State)


    let test<'a> name = TestCaseBuilder<'a> name


    type TestGroupState = {
        Name: string
        State: State
        Tests: AeneaTest list
        WithConfig: (Config -> Config) option
    }

    type TestGroupBuilder(name: string) =
        let mutable state : TestGroupState = {Name = name; State = Normal; Tests = []; WithConfig = None }

        member __.Zero () =
            state

        [<CustomOperation("max_test")>]
        member x.MaxTest(st, value) =
            let apply = fun cfg -> {cfg with MaxTest = value}
            x.WithConfig(st, apply)

        [<CustomOperation("quiet_on_success")>]
        member x.QuietOnSuccess(st, value) =
            let apply = fun cfg -> {cfg with QuietOnSuccess = value}
            x.WithConfig(st, apply)

        [<CustomOperation("with_config")>]
        member __.WithConfig(st, (value: Config -> Config)) =
            let newWithConfig =
                match state.WithConfig with
                | Some wc -> wc >> value
                | None -> value

            state <- {st with WithConfig = Some newWithConfig}
            state


        [<CustomOperation("focused")>]
        member __.Focused(st) =
            state <- {st with State = Focused}
            state

        [<CustomOperation("pending")>]
        member __.Pending(st) =
            state <- {st with State = Pending}
            state

        member __.Yield(other: unit) =
            state

        member __.Yield(code: AeneaTest) =
            state <- {state with Tests = (code :: state.Tests)}
            state

        member __.Combine(st: TestGroupState, ns: TestGroupState) =
            state


        member __.Delay f = f()

        member __.For(st, func) =
            __.Delay(fun () -> func ())

        member __.Run (state: TestGroupState) =
            TestGroup(state.Name, List.rev state.Tests, state.WithConfig, state.State)

    let testGroup name = TestGroupBuilder name
