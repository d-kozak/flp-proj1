# RKA 2 DKA
Autor: David Kozak
Contact: dkozak94@gmail.com
Year: 2017/2018

Projekt implementuje determinizaci rozsirenych konecnych automatu tak, jak byla popsana v zadani. 
Pomoci prepinace -i je mozne automat pouze nacist a vypsat.
Pomoci prepinace -t zapnete determinizaci

Program ocekava, ze na vstupu je syntakticky i semanticky korektni rozsireny automat. 

Rozsireni:
Program je schopny zpracovat i automaty, jejich stavy a symboly jsou retezce. 
Dale je tez mozne jednim spustenim zahajit zpracovani nekolika automatu, viz ./rka-2-tka -i input2 -t input2 -t input3 -i // tento posledni bude nacteny ze standardniho vstupu
    Nanestesti ale neni mozne v tomto pripade nacitat vice nez jeden automat ze standardniho vstupu, nebot funkce getContents neni reentrantni
