{
  open Lexing
  open Token

  type error = {
    character: char;
    line: int;
    pos: int;
  }

  exception Unexpected_character of error
}

(**********************************************************)

let letter = ['a'-'z']
let digit  = ['0'-'9']
let ascii  = _ # ['\n' '"']
let blanks = [' ' '\n' '\t']

rule tokenize = parse
  (* skip new lines and update line count (useful for error location) *)
  | '\n'
      { let _ = new_line lexbuf in tokenize lexbuf }

  (* skip other blanks *)
  | blanks
      { tokenize lexbuf }

  (* skip comments *)
  | '%' (_ # '\n')*
      { tokenize lexbuf }

  (* characters *)
  | '('
      { LP      :: tokenize lexbuf }
  | ')'
      { RP      :: tokenize lexbuf }
  | ':'
      { COLON      :: tokenize lexbuf }
  | ','
      { COMMA      :: tokenize lexbuf }
  | "->"
      { ARROW      :: tokenize lexbuf }
  | "STATES"
      { STATE_KW      :: tokenize lexbuf }
  | "SIGMA"
      { SIGMA_KW      :: tokenize lexbuf }
  | "BLANK"
      { BLANK_KW      :: tokenize lexbuf }
  | "GAMMA"
      { GAMMA_KW      :: tokenize lexbuf }
  | "START"
      { START_KW      :: tokenize lexbuf }
  | "ACCEPT"
      { ACCEPT_KW      :: tokenize lexbuf }
  | "DELTA"
      { DELTA_KW      :: tokenize lexbuf }
  | "WORD"
      { WORD_KW      :: tokenize lexbuf }
  | "TIME"
      { TIME_KW      :: tokenize lexbuf }
  | "RIGHT"
      { RIGHT_KW      :: tokenize lexbuf }
  | "LEFT"
      { LEFT_KW      :: tokenize lexbuf }
  | "STAY"
      { STAY_KW      :: tokenize lexbuf }

  | '"' (ascii* as lxm) '"'
      { TEXT lxm  :: tokenize lexbuf }
  | (digit+) as lxm
      { INTEGER (int_of_string lxm) :: tokenize lexbuf }

  (* end-of-file : end up with the empty stream *)
  | eof
      { [] }

  (* catch errors *)
  | _ as c
    {
      let e = {
          character = c;
          line = lexbuf.lex_curr_p.pos_lnum;
          pos  = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol;
        }
      in raise (Unexpected_character e)
    }
