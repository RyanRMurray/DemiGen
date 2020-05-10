# DemiGen


BUILD INSTRUCTIONS:

For the best performance, build with the following command: stack build --ghc-options=-O2

If profiling is required, go to the stack.yaml file and uncomment lines 68 to 70.

EXECUTION INSTRUCTIONS:

execute with the following command: stack exec DemiGen-exe

Follow the command-line instructions to generate maps. Maps are placed in the main project directory.

If profiling is required, enable the profiling flags and execute with the following command : stack exec -- DemiGen-exe +RTS [your flags here]