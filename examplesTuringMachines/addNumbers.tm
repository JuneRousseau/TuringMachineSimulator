% Turing Machine which compute the sum of 2 numbers (in unary format)
STATES: 0 1 2 3 4 5
SIGMA: "0" "1"
BLANK: "_"
GAMMA: "0" "1" "_"
START: 0
ACCEPT: 5
DELTA:

(0,"0") -> (1,"0",RIGHT)
(1,"1") -> (1,"1",RIGHT)
(1,"0") -> (2,"0",RIGHT)
(2,"1") -> (3,"0",LEFT)
(2,"0") -> (5,"_",RIGHT)
(3,"0") -> (4,"1",RIGHT)
(4,"1") -> (1,"0",LEFT)
(4,"0") -> (2,"0",RIGHT)


WORD: "0" "1" "1" "1" "1" "0"  "1" "1" "0"
TIME: 50
