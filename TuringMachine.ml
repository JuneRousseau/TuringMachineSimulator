type 'a symbol= Symbol of 'a
type 'a alphabet= 'a symbol list
type 'a state= State of 'a
type move= Left | Right | Stay
type ('a,'b) transition= ('b state * 'a symbol) * ('b state * 'a symbol * move)
(*Q, Sigma, Blank, Gamma, initial state, final states, transition table*)
type ('a,'b) turingMachine= ('b state list)*('a alphabet)*('a symbol)*('a alphabet)*('b state)*(('b state) list)*(('a,'b) transition list)

(*configuration= state, left strip , right strip *)
type ('a,'b) configuration= 'b state*('a symbol list)*('a symbol list)


let checkSymbol (s: 'a symbol) (blank : 'a symbol) (strip : 'a symbol list) : bool =
  match strip with
    | [] -> s=blank
    | x::r -> x=s

let shift (src : 'a symbol list) (dest : 'a symbol list) : (('a symbol list)*('a symbol list)) =
  match dest with
    | [] -> src,[]
    | x::r -> x::src,r

let applyTransition (t : ('a,'b) transition) (config: ('a,'b) configuration)  (m: ('a,'b) turingMachine) : (('a,'b) configuration) =
  let (start_state, start_symbol),(dest_state, writing_symbol, move)= t in
  let (current_state, left_strip, right_strip)= config in
  let (_,_,b,_,_,_,_)=m in
  match move with
    | Stay -> config
    | Left -> if start_state=current_state
              then (if (checkSymbol start_symbol b left_strip)
                    then (let new_left,new_right=(shift right_strip left_strip) in
                            match new_left with
                              | x::rest -> dest_state,(writing_symbol::rest),new_right
                              | _ -> dest_state,[writing_symbol],new_right
                    )
                    else config
                    )
              else config

    | Right -> if start_state=current_state
               then (if (checkSymbol start_symbol b right_strip)
                    then (let new_left,new_right=(shift left_strip right_strip) in
                            match new_right with
                              | x::rest -> dest_state,new_left,(writing_symbol::rest)
                              | _ -> dest_state,new_left,[writing_symbol]
                    )
                    else config
                    )
               else config



type 'a word = 'a symbol list
type time = int


let rec find_transition (config: ('a,'b) configuration) (delta : ('a,'b) transition list) (blank : 'a symbol): ('a,'b) transition option=
    match delta with
    | [] -> None
    | ((q,s),x)::rest ->
      let (cur_q,_,r)=config in
        (if q=cur_q
        then (match r with
                | [] -> if s=blank then Some ((q,s),x) else None
                | cur_s::_ ->
                    if cur_s=s
                    then Some ((q,s),x)
                    else find_transition config rest blank
                )
        else find_transition config rest blank)



let rec compute (config: ('a,'b) configuration) (m : ('a,'b) turingMachine) (t : time): bool =
  match t with
    | 0 -> false
    | _ ->  let (q,_,_)=config in
            let (_,_,b,_,_,f,delta)=m in
              match (find_transition config delta b) with
                | None ->  List.mem q f
                | Some tr -> let new_config = (applyTransition tr config m) in
                              compute new_config m (t-1)


let isAccepted (w: 'a word) (m : ('a,'b) turingMachine) (t : time): bool =
  let init_left=[] in
  let init_right=w in
  let (_,_,_,_,q_0,_,_)=m in
    compute (q_0,init_left,init_right) m t




(*EXAMPLES*)

let zNunN_states=[State 0;State 1;State 2;State 3;State 4];;
let sigma_bin=[Symbol "0";Symbol "1"];;
let blank_symb=Symbol "B";;
let gamma_bin=[Symbol "0";Symbol "1";Symbol "X";Symbol "X"; blank_symb];;
let start_state=State 0;;
let accept_state=[State 4];;
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
(State 3, Symbol "B"), (State 4, Symbol "B", Right)
]

let turingMachinezNunN=(zNunN_states,sigma_bin,blank_symb,gamma_bin,start_state, accept_state, delta);;

let wordTest=[Symbol "0";Symbol "0";Symbol "0";Symbol "1";Symbol "1";Symbol "1"];;

print_string (string_of_bool (isAccepted wordTest turingMachinezNunN 10000));;
