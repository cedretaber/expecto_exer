module ExpectoExer

open Expecto
open FsCheck

[<Tests>]
let tests =
  test "A simple test" {
    let subject = "Hello World"
    Expect.equal subject "Hello World" "The strings should equal"
  }

[<Tests>]
let funTest =
  testCase "A simple test 2" <| fun () ->
    let expected = 4
    Expect.equal expected (2+2) "2+2 should equal 4"

[<Tests>]
let tests2 =
  testList "A test group" [
    test "one test" {
      Expect.equal (2+2) 4 "2+2"
    }

    test "another test that fails" {
      Expect.equal (3+3) 5 "3+3"
    }

    testAsync "this is an async test" {
      let! x = async { return 4 }
      Expect.equal x (2+2) "2+2"
    }
  ]

[<PTests>]
let skippedTestFromReflectionDiscovery =
  testCase "skipped" <| fun () ->
    Expect.equal (2+2) 4 "2+2"

[<Tests>]
let myTests =
  testList "normal" [
    testList "unfocused list" [
      ptestCase "skipped" <| fun () -> Expect.equal (2+2) 1 "2+2?"
      testCase "will run" <| fun () -> Expect.equal (2+2) 4 "2+2"
      ptest "skipped" { Expect.equal (2+2) 1 "2+2?" }
      ptestAsync "skipped async" { Expect.equal (2+2) 1 "2+2?" }
    ]
    testCase "will run" <| fun () -> Expect.equal (2+2) 4 "2+2"
    ptestCase "skipped" <| fun () -> Expect.equal (2+2) 1 "2+2?"
    ptestList "skipped list" [
      testCase "skipped" <| fun () -> Expect.equal (2+2) 1 "2+2?"
      ftest "skipped" { Expect.equal (2+2) 1 "2+2?" }
    ]
  ]

[<Tests>]
let parametisedTests =
  testList "numberology 101" (
    testParam 1333 [
      "First sample",
        fun value () ->
          Expect.equal value 1333 "Should be expected value"
      "Second sample",
        fun value () ->
          Expect.isLessThan value 1444 "Should be less than"
  ] |> List.ofSeq)

let propertyBasedTests =
  let config = { FsCheckConfig.defaultConfig with maxTest = 10000 }
  testList "FsCheck samples" [
    testProperty "Addition is commutative" <| fun a b ->
      a + b = b + a

    testProperty "Reverse of reverse of a list is the original list" <|
      fun (xs:list<int>) -> List.rev (List.rev xs) = xs

    // you can also override the FsCheck config
    testPropertyWithConfig config "Product is distributive over addition" <|
      fun a b c ->
        a * (b + c) = a * b + a * c
  ]

type User = {
    Id : int
    FirstName : string
    LastName : string
}

type UserGen() =
   static member User() : Arbitrary<User> =
       let genFirsName = Gen.elements ["Don"; "Henrik"; null]
       let genLastName = Gen.elements ["Syme"; "Feldt"; null]
       let createUser id firstName lastName =
           {Id = id; FirstName = firstName ; LastName = lastName}
       let getId = Gen.choose(0,1000)
       let genUser =
           createUser <!> getId <*> genFirsName <*> genLastName
       genUser |> Arb.fromGen

[<Tests>]
let propertyBasedTests2 =
  let config = { FsCheckConfig.defaultConfig with arbitrary = [typeof<UserGen>] }
  testList "FsCheck samples" [
    
    // you can also override the FsCheck config
    testPropertyWithConfig config "User with generated User data" <|
      fun x ->
        Expect.isNotNull x.FirstName "First Name should not be null"
  ]

[<EntryPoint>]
let main args =
  runTestsInAssembly defaultConfig args