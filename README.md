# Turing Machine Simulator
The simulator prints the state of the Turing Machine and execute it since:
- the machine times out
- the ACCEPT state is reached

# Compilation and usage

Dependencies:
- camlp4
- ocamlbuild

To compile the project: `make main.native`

To use the simulator: `./main.native < $turingMachineFile`

Examples of Turing Machine specification are given in the directory
*exampleTuringMachines*
