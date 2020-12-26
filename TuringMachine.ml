type symbol= Symbol of string
type alphabet= symbol list
type state= State of int
type move= Left | Right | Stay
type transition= (state * symbol) * (state * symbol * move)
(*Q, Sigma, Blank, Gamma, initial state, final states, transition table*)
type turingMachine= (state list)*(alphabet)*(symbol)*(alphabet)*(state)*((state) list)*(transition list)

(*configuration= state, left strip , right strip *)
type configuration= state*(symbol list)*(symbol list)

let string_of_move (m : move) : string =
 match m with
  | Left -> "Left"
  | Right -> "Right"
  | Stay -> "Stay"

let string_of_symbol (s : symbol) : string =
  match s with
  | Symbol str -> str

let string_of_state (s : state) : string =
  match s with
  | State n -> string_of_int n

let rec string_of_strip (strip : symbol list) : string=
  List.fold_left (^) "" (List.map (fun s -> " "^(string_of_symbol s)^"  ")  strip)

let string_of_transition (t : transition) : string =
  let (q_s, s_s),(q_d,s_d,m)= t in
  "{( State "^string_of_state q_s^", "^string_of_symbol s_d^") --> ( State "^string_of_state q_d^", "^string_of_symbol s_d^", "^string_of_move m^")}"

let string_of_configuration (config: configuration) : string =
  let (q,l,r)=config in
  match r with
  | [] -> "( State "^(string_of_state q)^", Strip |...  _   _  "^string_of_strip (List.rev l)^"[_]  _  ...|  )"
  | x::t -> "( State "^(string_of_state q)^", Strip |...  _   _  "^string_of_strip (List.rev l)^"["^(string_of_symbol x)^"] "^string_of_strip t^" _   _  ...| )"



let checkSymbol (s: symbol) (blank : symbol) (strip : symbol list) : bool =
  match strip with
    | [] -> s=blank
    | x::r -> x=s

let shift_left (left : symbol list) (right : symbol list) (blank : symbol): ((symbol list)*(symbol list)) =
  match left with
    | [] -> [], blank::right
    | x::r -> r,x::right

let shift_right (left : symbol list) (right : symbol list) (blank : symbol): ((symbol list)*(symbol list)) =
  match right with
    | [] -> blank::left,[]
    | x::r -> x::left,r


let applyTransition (t : transition) (config: configuration)  (m: turingMachine) : (configuration) =
  let (start_state, start_symbol),(dest_state, writing_symbol, move)= t in
  let (current_state, left_strip, right_strip)= config in
  let (_,_,b,_,_,_,_)=m in
  match move with
    | Stay -> dest_state,left_strip,right_strip
    | Left -> if start_state=current_state
              then (if (checkSymbol start_symbol b right_strip)
                    then (let right_strip=
                            (match right_strip with
                              | _::rest -> (writing_symbol::rest)
                              | _ -> [writing_symbol]) in
                            let new_left,new_right=(shift_left left_strip right_strip b) in
                            dest_state,new_left,new_right)
                    else config
                    )
              else config

    | Right -> if start_state=current_state
               then (if (checkSymbol start_symbol b right_strip)
                    then (let right_strip=
                      (match right_strip with
                        | _::rest -> (writing_symbol::rest)
                        | _ -> [writing_symbol]) in
                      let new_left,new_right=(shift_right left_strip right_strip b) in
                      dest_state,new_left,new_right
                    )
                    else config
                    )
               else config

type word = symbol list
type time = int

(*find which transition is applyable (only works with deterministic Turing Machine)*)
let rec find_transition (config: configuration) (delta : transition list) (blank : symbol): transition option=
    match delta with
    | [] -> None (*base case : the transition list is empty*)
    | ((q,s),x)::rest -> (*recursive case : there exist a transition from the state q and the symbol s*)
      let (cur_q,_,r)=config in
        (if q=cur_q (*the current state is the same as the transition found*)
        then (match r with  (*check the symbol*)
                | [] -> if s=blank then Some ((q,s),x) else find_transition config rest blank
                | cur_s::_ ->
                    if cur_s=s
                    then Some ((q,s),x)
                    else find_transition config rest blank
                )
        else find_transition config rest blank)


(*compute the next configuration of the Turing Machine m from the current c
onfiguration config *)
let rec compute (config: configuration) (m : turingMachine) (t : time): bool =
  let debug= print_string ("t: "^(string_of_int t)^"\t"^(string_of_configuration config)^"\n") in
  match t with
    | 0 -> false
    | _ ->  let (q,_,_)=config in
            let (_,_,b,_,_,f,delta)=m in
              match (find_transition config delta b) with
                | None ->  List.mem q f
                | Some tr ->  let new_config = (applyTransition tr config m) in
                              compute new_config m (t-1)

(*a word w is accepted in a time t by the Turing Machine m if it is possible to
reach a final state with the initial configuration (_,w) in less than t
iterations*)
let isAccepted (w: word) (m : turingMachine) (t : time): bool =
  let init_left=[] in
  let init_right=w in
  let (_,_,_,_,q_0,_,_)=m in
    compute (q_0,init_left,init_right) m t



(*EXAMPLES

let zNunN_states=[State 0;State 1;State 2;State 3;State 4];;
let sigma_bin=[Symbol "0";Symbol "1"];;
let blank_symb=Symbol "B";;
let gamma_bin=[Symbol "0";Symbol "1";Symbol "X";Symbol "Y"; blank_symb];;
let start_state=State 0;;
let accept_state=[State 6];;
let delta=[
(State 0, Symbol "0"), (State 1, Symbol "X", Right);
(State 0, Symbol "Y"), (State 3, Symbol "Y", Right);
(State 1, Symbol "0"), (State 1, Symbol "0", Right);
(State 1, Symbol "1"), (State 2, Symbol "Y", Left);
(State 1, Symbol "Y"), (State 1, Symbol "Y", Right);
(State 2, Symbol "0"), (State 2, Symbol "0", Left);
(State 2, Symbol "X"), (State 0, Symbol "X", Right);
(State 2, Symbol "Y"), (State 2, Symbol "Y", Left);
(State 3, Symbol "Y"), (State 3, Symbol "Y", Right);
(State 3, blank_symb), (State 4, blank_symb, Right);
]

let turingMachinezNunN=(zNunN_states,sigma_bin,blank_symb,gamma_bin,start_state, accept_state, delta);;

let wordTest=[Symbol "0";Symbol "0";Symbol "0";Symbol "1";Symbol "1";Symbol "1"];;

print_string (string_of_bool (isAccepted wordTest turingMachinezNunN 50));;*)
