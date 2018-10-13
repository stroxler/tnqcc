open Cmdliner


let helloCmd =
  ( Term.(const (fun () -> print_string "Hello from cmdliner")
          $ const ())
  , Term.info "hello"
      ~doc:"Say hello"
      ~man:[ `S "Description"
           ; `P"$(b,hello) is really just a demo of no-arg functions in Cmdliner"
           ]
  )


let in_path_arg =
  let doc = "Path of input file (defaults to stdin)" in
  let docv = "IN_PATH" in
  Arg.(value & opt string "-" & info ["i"; "in-path"] ~docv ~doc)

let out_path_arg =
  let doc = "Path of output file (defaults to stdout)" in
  let docv = "OUT_PATH" in
  Arg.(value & opt string "-" & info ["o"; "out-path"] ~docv ~doc)


let lexCmd =
  let go in_path out_path = Tnqcc.run Tnqcc.lex_chan ~in_path ~out_path in
  ( Term.(const go
          $ in_path_arg
          $ out_path_arg)
  , Term.info "lex"
      ~doc:"Lex a file of C code"
      ~man:[ `S "Description"
           ; `P"$(b,lex) will print tokens after lexing code"
           ]
  )


let parseCmd =
  let go in_path out_path = Tnqcc.run Tnqcc.parse_chan ~in_path ~out_path in
  ( Term.(const go
          $ in_path_arg
          $ out_path_arg)
  , Term.info "parse"
      ~doc:"Parse a file of C code"
      ~man:[ `S "Description"
           ; `P"$(b,parse) will print an AST after parsing code"
           ]
  )


let genCmd =
  let go in_path out_path = Tnqcc.run Tnqcc.gen_chan ~in_path ~out_path in
  ( Term.(const go
          $ in_path_arg
          $ out_path_arg)
  , Term.info "gen"
      ~doc:"Generate assembly from C code"
      ~man:[ `S "Description"
           ; `P"$(b,gen) will print generated assembly instructions"
           ]
  )


let all = [
  lexCmd;
  parseCmd;
  genCmd;
  helloCmd;
]

let default =
  let doc = "Tnqcc suite" in
  ( Term.(ret (const (`Help (`Pager, None))))
  , Term.info "tnqcc" ~doc
      ~man:
        [ `S "Description"
        ; `P {|The $(b,tnqcc) command line tools provide functions for compiling
               a subset of C. They are Trox's adaptation of Nora Sandler's nqcc
               project.|}
        ]
  )

let _ = Term.exit @@ Term.eval_choice default all ~catch:true
