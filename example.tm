% Turing Machine which accepts {0^n 1^n | n>=0}
STATES: 0 1 2 3 4
SIGMA: "0" "1"
BLANK: "B"
GAMMA: "0" "1" "X" "Y" "B"
START: 0
ACCEPT: 4
DELTA:

(0,"0") -> (1,"X",RIGHT)
(0,"Y") -> (3,"Y",RIGHT)
(1,"0") -> (1,"0",RIGHT)
(1,"1") -> (2,"Y",LEFT)
(1,"Y") -> (1,"Y",RIGHT)
(2,"0") -> (2,"0",LEFT)
(2,"X") -> (0,"X",RIGHT)
(2,"Y") -> (2,"Y",LEFT)
(3,"Y") -> (3,"Y",RIGHT)
(3,"B") -> (4,"B",RIGHT)

WORD: "0" "0" "0" "1" "1" "1"
TIME: 50
