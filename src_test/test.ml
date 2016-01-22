open OUnit
open MiniKanrenImpl

module Test1 = struct
  let test_int ctxt =
    assert_equal ~printer:string_of_int 1 1
end


let suite = "Test ppx_morphism" >:::
  [ "test_int_record" >:: Test1.test_int
  ]


let _ =
  run_test_tt_main suite
