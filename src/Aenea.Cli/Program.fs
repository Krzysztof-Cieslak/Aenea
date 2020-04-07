// Learn more about F# at http://fsharp.org

open System
open Aenea
open FsCheck
open CeAPI

[<EntryPoint>]
let main argv =
    // let t =
    //     testGroup "tttt" [
    //         test "1" (fun (xs:list<int>) -> List.rev(List.rev xs) = xs)

    //         withConfig (fun c -> {c with MaxTest = 12}) (
    //             ftestGroup "tttt" [
    //                 test "2" (fun (xs:list<int>) -> List.rev(List.rev xs) = xs)
    //                 ptest "3" (fun (xs:list<int>) -> List.rev(List.rev xs) = xs)
    //                 withConfig (fun c ->  {c with MaxTest = 42})
    //                     (test "42" (fun (xs:list<int>) -> List.rev(List.rev xs) = xs))
    //             ])

    //         withConfig (fun c -> {c with MaxTest = 42})
    //             (test "4" (fun (xs:list<int>) -> List.rev(List.rev xs) = xs))

    //         fexample "example Test" (fun () -> 3 = 3)
    //     ]
    // run t

    let t =
        test "My test" {
            example

            code (fun (xs:list<int>) ->
                List.rev(List.rev xs) <> xs)
        }
    run t

    // printfn "%A" t
    0 // return an integer exit code
