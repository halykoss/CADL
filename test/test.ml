open OUnit2

let empty_list = []
let list_a = [1;2;3]

let test_list_length _ =
  (* Check if the list is empty. *)
  assert_equal 0 (List.length empty_list);
  (* Check if a given list contains 3 elements. *)
  assert_equal 3 (List.length list_a)

let test_list_append _ =
  let list_b = List.append empty_list [1;2;3] in
  assert_equal list_b list_a

let suite =
  "ExampleTestList" >::: [
    "test_list_length" >:: test_list_length;
    "test_list_append" >:: test_list_append
  ]

let () =
  run_test_tt_main suite