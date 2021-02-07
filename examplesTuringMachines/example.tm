% Turing Machine which accepts {0^n 1^n | n>=0}
STATES: 0 1 2 3 4 5 6
SIGMA: "0" "1"
BLANK: "_"
GAMMA: "0" "1" "X" "Y" "_"
START: 0
ACCEPT: 6
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
(3,"_") -> (4,"_",LEFT)
(4,"_") -> (4,"_",LEFT)
(4,"0") -> (4,"0",LEFT)
(4,"1") -> (4,"1",LEFT)
(4,"Y") -> (4,"Y",LEFT)
(4,"X") -> (5,"X",LEFT)
(5,"X") -> (5,"X",LEFT)
(5,"_") -> (6,"_",STAY)

WORD: "0" "0" "0" "1" "1" "1"
TIME: 50
