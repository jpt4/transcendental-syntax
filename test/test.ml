(* FIXME *)

open Alcotest
open Base
open Lsc.Lsc_ast
open Lsc.Lsc_parser
open Lsc.Lsc_lexer

let test filename expected () =
  let lexbuf = Lexing.from_channel (Stdlib.open_in filename) in
  let mcs = constellation_file read lexbuf in
  let result =
    exec ~showtrace:false
         ~showsteps:false mcs
         |> kill
         |> string_of_constellation in
  check string "same string" result expected

let example filename = "../examples/" ^ filename

let suite =
  [ "Automata", `Quick, test (example "automata/nfsa_ending00.stellar") "accept;"
  ; "Prolog", `Quick, test (example "logicprogramming/prolog.stellar") "s(s(s(s(0))));"
  ; "MLL (cut-elim)", `Quick, test (example "mll/cut.stellar") "6(X7) 3(X7);"
  ]

let () =
  Alcotest.run "Stellar Resolution" [ "Basic tests", suite ]
