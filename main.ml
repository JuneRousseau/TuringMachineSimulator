open Lexer
open TuringMachine

let _ =
  try
    (* lexical and syntactic analysis *)
    let lexbuf = Lexing.from_channel stdin in
    let token_stream = Stream.of_list (Lexer.tokenize lexbuf) in
    let tm,w,t= Parser.parse_spec token_stream in

    (* Print LLVM IR *)
    if isCorrectTuringMachine tm
    then let accepted = isAccepted w tm t in print_endline (string_of_bool accepted)
    else Printf.printf "The given specification isn't a correct Turing Machine\n" ; exit 1

  with
    Lexer.Unexpected_character e ->
    Printf.printf "Unexpected character: `%c' at position '%d' on line '%d'\n"
		  e.character e.pos e.line;
    exit 1
