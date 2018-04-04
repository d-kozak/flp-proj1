# RKA 2 DKA
# Autor: David Kozak
# Contact: dkozak94@gmail.com
# Year: 2017/2018

all:
	ghc -o rka-2-dka src/Main.hs src/FiniteAutomata.hs src/FaAlgorithms.hs src/ArgsParser.hs

pack:
	zip flp-fun-xkozak15.zip Makefile README src/*.hs test/*.hs