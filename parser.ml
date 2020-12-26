open TuringMachine
open Token

(* p? *)
let opt p = parser
  | [< x = p >] -> Some x
  | [<>] -> None

(* p* *)
let rec many p = parser
  | [< x = p; l = many p >] -> x :: l
  | [<>] -> []

(* p+ *)
let some p = parser
  | [< x = p; l = many p >] -> x :: l

let parse_integer= parser
  | [< 'INTEGER n>] -> n

let parse_symbol= parser
  | [< 'TEXT str>] -> str

let rec parse_states= parser
  | [< 'STATE_KW ; 'COLON ; states=some parse_integer >] -> List.map (fun n -> State n) states

and parse_sigma= parser
  | [<'SIGMA_KW ; 'COLON ; sigma=some parse_symbol>] -> List.map (fun s -> Symbol s) sigma

and parse_blank= parser
  | [<'BLANK_KW ; 'COLON ; blank= parse_symbol>] -> Symbol blank

and parse_gamma= parser
  | [<'GAMMA_KW ; 'COLON ; gamma=some parse_symbol>] -> List.map (fun s -> Symbol s) gamma

and parse_start= parser
  | [<'START_KW ; 'COLON ; 'INTEGER n>] -> State n

and parse_accept= parser
  | [<'ACCEPT_KW ; 'COLON ; accepts=some parse_integer >] -> List.map (fun n -> State n) accepts

and parse_delta= parser
  | [<'DELTA_KW ; 'COLON ; rules=some parse_transition>] -> rules

and parse_transition = parser
  | [<'LP ; 'INTEGER q_s; 'COMMA  ; 'TEXT s_s ;'RP; 'ARROW ;'LP ;'INTEGER q_d; 'COMMA  ; 'TEXT s_d ; 'COMMA  ; m=parse_move ; 'RP>] -> (State q_s,Symbol s_s),(State q_d,Symbol s_d,m)

and parse_move = parser
  | [<'LEFT_KW>] -> Left
  | [<'RIGHT_KW>] -> Right
  | [<'STAY_KW>] -> Stay

and parse_word = parser
  | [<'WORD_KW ; 'COLON ; gamma=some parse_symbol>] -> List.map (fun s -> Symbol s) gamma

and parse_time= parser
  | [<'TIME_KW ; 'COLON ; 'INTEGER n>] -> n

and parse_spec = parser
  | [<states= parse_states ; sigma=parse_sigma ; blank=parse_blank ; gamma= parse_gamma ; start=parse_start ; accept=parse_accept ; delta=parse_delta ; word=parse_word ; time=parse_time>] ->
      (states,sigma,blank,gamma,start, accept,delta), word, time
