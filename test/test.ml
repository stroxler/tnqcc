open OUnit2

let suite =
    "All" >::: [
        "Lex" >::: Test_lex.tests;
        "Parse" >::: Test_parse.tests;
        "Context" >::: Test_context.tests;
    ]

let () = run_test_tt_main suite
