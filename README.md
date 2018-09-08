# RKA 2 DKA
Autor: David Kozak
Contact: dkozak94@gmail.com
Year: 2017/2018

Project implements finite automata determinization. 

## Usage 
	-i - just load the automata and print it again
	-t - load the automata and determinize it

## Extensions ( not part of the assignment specification)
* Program can also handle automata, whose names and symbols are strings, not numbers. In the end the all states are renamed to numbers
* It is possible to process multiple automata files at once, viz ./rka-2-tka -i input1 -t input2 -t input3 -i // in this case the last one will be loaded from standard output
* if you specify -e, the automata is loaded and epsilon rules are removed
* there are some extra unit tests
* if you specify -d, the output format is changed so that it reflects the facts that the new states are actually macrostates composed of the states of the original automata. Therefore every state is specified 
as a set. The ouput then looks as follows:
''' {0,1},{0},{2}
    0
    {2}
    {0,1},a,{0,1}
    {0,1},b,{2}
    {0},a,{0,1}
    {0},b,{2} '''

