open OUnit
open MiniKanrenImpl

module Logger = UnitLogger
module M = MiniKanrenImpl.Make(Logger)

module Test1 = struct
  let test_int ctxt =
    assert_equal ~printer:string_of_int 1 1

  let test2 ctxt =
    ()
    (* let l = Logger.create l in *)
    (* let n = Logger.make_node l in *)

    (* let s1 = (State.empty(), l, n) in *)

    (* let f st = ( *)
end


let suite = "Test ppx_morphism" >:::
  [ "test_int_record" >:: Test1.test_int
  ; "" >:: Test1.test2
  ]


let _ =
  run_test_tt_main suite
