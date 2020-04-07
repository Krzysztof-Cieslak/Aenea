namespace Aenea

open FsCheck

module Model =
    type State =
        | Normal
        | Focused
        | Pending

    type Test =
        | TestGroup of label: string * tests: Test list * config : Config option * state: State
        | TestCase of label: string * testCode: (Config -> unit) * config: Config option * state: State

[<AutoOpen>]
module Api =
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

    let withConfig (configTransform: Config -> Config) (t: Test) =
        match t with
        | TestGroup (l, t, oldConfig, state) ->
            let newConfig =
                oldConfig
                |> Option.defaultValue Config.Default
                |> configTransform
            TestGroup(l,t,Some newConfig, state)
        | TestCase(label, testCode, oldConfig, state) ->
            let newConfig =
                oldConfig
                |> Option.defaultValue Config.Default
                |> configTransform

            TestCase(label, testCode, Some newConfig, state)

    let flattenTests (test: Test) =
        let rec helper (label: string) (config: Config option) (parentState: State) (t: Test) =
            match t with
            | TestCase(name, code, cfg, state) ->
                let newState =
                    match state, parentState with
                    | Normal, _ -> parentState
                    | _, _ -> state
                let c =
                    match config, cfg with
                    | _, Some c -> Some c
                    | Some c, None -> Some c
                    | _ -> None
                List.singleton (label + "/" + name, code, c, newState)
            | TestGroup(name, tests, cfg, state) ->
                let newState =
                    match state, parentState with
                    | Normal, _ -> parentState
                    | _, _ -> state
                let c =
                    match config, cfg with
                    | _, Some c -> Some c
                    | Some c, None -> Some c
                    | _ -> None
                tests |> List.collect (helper (label + "/" + name) c newState)
        helper "" None Normal test

    let run (test: Test) =
        let flatted = flattenTests test
        let isAnyFocused = flatted |> List.exists (fun (_,_,_,s) -> s = Focused)
        let flatted =
            if isAnyFocused then
                flatted |> List.filter (fun (_,_,_,s) -> s = Focused)
            else
                flatted
        for (name, f, cfg, state) in flatted do
            if state = Pending then
                ()
            else
                let c = Option.defaultValue Config.Default cfg
                let c = {c with Name = name}
                f c

module CeAPI =
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
        Config: Config
        TestCode: ('a -> bool) option
        IsExample: bool
    }

    type TestCaseBuilder<'a> (name: string) =
        let mutable state : TestCaseState<'a> = {Name = name; State = Normal; Config = Config.Default; TestCode = None; IsExample = false}

        member __.Zero () =
            state

        [<CustomOperation("max_test")>]
        member __.MaxTest(st, value) =
            state <- {st with Config = {state.Config with MaxTest = value}}
            state

        [<CustomOperation("code")>]
        member __.Code(st, value) =
            state <- {st with TestCode = Some value}
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

        member __.Yield(code: 'a -> bool) =
            state <- {state with TestCode = Some code}
            state

        member __.Delay f = f()

        member __.For(st, func) =
            __.Delay(fun () -> func ())

        member __.Run (state: TestCaseState<'a>) =
            let quickCheckTest =
                match state.TestCode with
                | Some tc ->
                    if state.IsExample then
                        fun cfg -> Check.One({cfg with MaxTest = 1}, tc)
                    else
                        fun cfg -> Check.One(cfg, tc)
                | None -> failwith "Some test code needs to be returned"
            TestCase(state.Name, quickCheckTest, Some state.Config, state.State)


    let test<'a> name = TestCaseBuilder<'a> name



