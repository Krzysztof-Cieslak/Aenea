// Learn more about F# at http://fsharp.org

open System
open Aenea
open FsCheck
open ListDSL
open CeDSL

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

    let group =
        testGroup "My group" {
            max_test 1000 // with_config (fun c -> {c with MaxTest = 1000})
            quiet_on_success true

            test "1" {
                max_test 10
                quiet_on_success false

                fun (xs:list<int>) ->
                    List.rev(List.rev xs) = xs
            }

            test "2" {
                fun (xs:list<int>) ->
                   List.rev(List.rev xs) = xs
            }

            testGroup "My other group" {
                max_test 12

                test "3" {
                    max_test 42
                    quiet_on_success false

                    fun (xs:list<int>) ->
                        List.rev(List.rev xs) = xs
                }
                test "4" {
                    fun (xs:list<int>) ->
                        List.rev(List.rev xs) = xs
                }
            }
            test "5" {
                example
                quiet_on_success false

                fun (xs:list<int>) ->
                    List.rev(List.rev xs) = xs
            }

            testGroup "FsCheck Properties" {
                test "imply" {
                    fun (x:int) ->
                       (x % 2 = 0) ==> lazy ((x * 2) % 2 = 0)
                }
                test "labels" {
                    pending // uncomment to see failures with printed labels
                    fun (x:int) ->
                        (x % 2 = 0) |@ "not even"
                        .&.
                        (sign x = 1) |@ "not positive"
                }
            }
        }
    run group

    // printfn "%A" group
    0 // return an integer exit code
