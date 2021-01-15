%Complicated Wires

solveComplicatedWires(Red, Blue, Star, Light) :- Red = 1, Blue = 1, Star = 1, Light = 1, write('Do not cut the wire').
solveComplicatedWires(Red, Blue, Star, Light) :- Red = 1, Blue = 1, Star = 1, Light \= 1, write('Cut the wire if the bomb has a parallel port').
solveComplicatedWires(Red, Blue, Star, Light) :- Red = 1, Blue = 1, Star \= 1, Light = 1, write('Cut the wire if the last digit of the serial number is even').
solveComplicatedWires(Red, Blue, Star, Light) :- Red = 1, Blue = 1, Star \= 1, Light \= 1, write('Cut the wire if the last digit of the serial number is even').
solveComplicatedWires(Red, Blue, Star, Light) :- Red = 1, Blue \= 1, Star = 1, Light = 1, write('Cut the wire if the bomb has two or more batteries').
solveComplicatedWires(Red, Blue, Star, Light) :- Red = 1, Blue \= 1, Star = 1, Light \= 1, write('Cut the wire').
solveComplicatedWires(Red, Blue, Star, Light) :- Red = 1, Blue \= 1, Star \= 1, Light = 1, write('Cut the wire if the bomb has two or more batteries').
solveComplicatedWires(Red, Blue, Star, Light) :- Red = 1, Blue \= 1, Star \= 1, Light \= 1, write('Cut the wire if the last digit of the serial number is even').
solveComplicatedWires(Red, Blue, Star, Light) :- Red \= 1, Blue = 1, Star = 1, Light = 1, write('Cut the wire if the bomb has a parallel port').
solveComplicatedWires(Red, Blue, Star, Light) :- Red \= 1, Blue = 1, Star = 1, Light \= 1, write('Do not cut the wire').
solveComplicatedWires(Red, Blue, Star, Light) :- Red \= 1, Blue = 1, Star \= 1, Light = 1, write('Cut the wire if the bomb has a parallel port').
solveComplicatedWires(Red, Blue, Star, Light) :- Red \= 1, Blue = 1, Star \= 1, Light \= 1, write('Cut the wire if the last digit of the serial number is even').
solveComplicatedWires(Red, Blue, Star, Light) :- Red \= 1, Blue \= 1, Star = 1, Light = 1, write('Cut the wire if the bomb has two or more batteries').
solveComplicatedWires(Red, Blue, Star, Light) :- Red \= 1, Blue \= 1, Star = 1, Light \= 1, write('Cut the wire').
solveComplicatedWires(Red, Blue, Star, Light) :- Red \= 1, Blue \= 1, Star \= 1, Light = 1, write('Do not cut the wire').
solveComplicatedWires(Red, Blue, Star, Light) :- Red \= 1, Blue \= 1, Star \= 1, Light \= 1, write('Cut the wire').

%Passwords

length([], 0).
length([_|T], L) :- length(T, L1), L is L1 + 1.

listIndex([H|T], 0, H).
listIndex([H|T], I, E) :- I1 is I - 1, listIndex(T, I1, E).

member([H|_], H).
member([_|T], M) :- member(T, M).

append([], X, X).
append([X|Y], Z, [X|W]) :- append(Y, Z, W).

word([a, b, o, u, t]).
word([a, f, t, e, r]).
word([a, g, a, i, n]).
word([b, e, l, o, w]).
word([c, o, u, l, d]).
word([e, v, e, r, y]).
word([f, i, r, s, t]).
word([f, o, u, n, d]).
word([g, r, e, a, t]).
word([h, o, u, s, e]).
word([l, a, r, g, e]).
word([l, e, a, r, n]).
word([n, e, v, e, r]).
word([o, t, h, e, r]).
word([p, l, a, c, e]).
word([p, l, a, n, t]).
word([p, o, i, n, t]).
word([r, i, g, h, t]).
word([s, m, a, l, l]).
word([s, o, u, n, d]).
word([s, p, e, l, l]).
word([s, t, i, l, l]).
word([s, t, u, d, y]).
word([t, h, e, i, r]).
word([t, h, e, r, e]).
word([t, h, e, s, e]).
word([t, h, i, n, g]).
word([t, h, i, n, k]).
word([t, h, r, e, e]).
word([w, a, t, e, r]).
word([w, h, e, r, e]).
word([w, h, i, c, h]).
word([w, o, r, l, d]).
word([w, o, u, l, d]).
word([w, r, i, t, e]).

solvePassword(List1, List2, List3, List4, List5, Word) :-
    member(List1, E1),
    member(List2, E2),
    member(List3, E3),
    member(List4, E4),
    member(List5, E5),
    Word = [E1, E2, E3, E4, E5],
    word(Word).

%Simon Says

isVowel(a).
isVowel(e).
isVowel(i).
isVowel(o).
isVowel(u).

serialHasVowel([]) :- isVowel(m).
serialHasVowel([H|T]) :- isVowel(H).
serialHasVowel([H|T]) :- -isVowel(H), serialHasVowel(T).

-isVowel(H) :- not isVowel(H).

test(SerialNumber, Strikes, Color) :- serialHasVowel(SerialNumber), Strikes is 0, Color = red.

-serialHasVowel(SerialNumber) :- not serialHasVowel(SerialNumber).

%Vowel, 0 strikes
solveSimon(SerialNumber, Strikes, Color) :- serialHasVowel(SerialNumber), Strikes is 0, Color = red, write('Press Blue').
solveSimon(SerialNumber, Strikes, Color) :- serialHasVowel(SerialNumber), Strikes is 0, Color = blue, write('Press Red').
solveSimon(SerialNumber, Strikes, Color) :- serialHasVowel(SerialNumber), Strikes is 0, Color = green, write('Press Yellow').
solveSimon(SerialNumber, Strikes, Color) :- serialHasVowel(SerialNumber), Strikes is 0, Color = yellow, write('Press Green').

%Vowel, 1 strike
solveSimon(SerialNumber, Strikes, Color) :- serialHasVowel(SerialNumber), Strikes is 1, Color = red, write('Press Yellow').
solveSimon(SerialNumber, Strikes, Color) :- serialHasVowel(SerialNumber), Strikes is 1, Color = blue, write('Press Green').
solveSimon(SerialNumber, Strikes, Color) :- serialHasVowel(SerialNumber), Strikes is 1, Color = green, write('Press Blue').
solveSimon(SerialNumber, Strikes, Color) :- serialHasVowel(SerialNumber), Strikes is 1, Color = yellow, write('Press Red').

%Vowel, 2 strikes
solveSimon(SerialNumber, Strikes, Color) :- serialHasVowel(SerialNumber), Strikes is 2, Color = red, write('Press Green').
solveSimon(SerialNumber, Strikes, Color) :- serialHasVowel(SerialNumber), Strikes is 2, Color = blue, write('Press Red').
solveSimon(SerialNumber, Strikes, Color) :- serialHasVowel(SerialNumber), Strikes is 2, Color = green, write('Press Yellow').
solveSimon(SerialNumber, Strikes, Color) :- serialHasVowel(SerialNumber), Strikes is 2, Color = yellow, write('Press Blue').

%No Vowel, 0 strikes
solveSimon(SerialNumber, Strikes, Color) :- -serialHasVowel(SerialNumber), Strikes is 0, Color = red, write('Press Blue').
solveSimon(SerialNumber, Strikes, Color) :- -serialHasVowel(SerialNumber), Strikes is 0, Color = blue, write('Press Yellow').
solveSimon(SerialNumber, Strikes, Color) :- -serialHasVowel(SerialNumber), Strikes is 0, Color = green, write('Press Green').
solveSimon(SerialNumber, Strikes, Color) :- -serialHasVowel(SerialNumber), Strikes is 0, Color = yellow, write('Press Red').

%No Vowel, 1 strike
solveSimon(SerialNumber, Strikes, Color) :- -serialHasVowel(SerialNumber), Strikes is 1, Color = red, write('Press Red').
solveSimon(SerialNumber, Strikes, Color) :- -serialHasVowel(SerialNumber), Strikes is 1, Color = blue, write('Press Blue').
solveSimon(SerialNumber, Strikes, Color) :- -serialHasVowel(SerialNumber), Strikes is 1, Color = green, write('Press Yellow').
solveSimon(SerialNumber, Strikes, Color) :- -serialHasVowel(SerialNumber), Strikes is 1, Color = yellow, write('Press Green').

%No Vowel, 2 strikes
solveSimon(SerialNumber, Strikes, Color) :- -serialHasVowel(SerialNumber), Strikes is 2, Color = red, write('Press Yellow').
solveSimon(SerialNumber, Strikes, Color) :- -serialHasVowel(SerialNumber), Strikes is 2, Color = blue, write('Press Green').
solveSimon(SerialNumber, Strikes, Color) :- -serialHasVowel(SerialNumber), Strikes is 2, Color = green, write('Press Blue').
solveSimon(SerialNumber, Strikes, Color) :- -serialHasVowel(SerialNumber), Strikes is 2, Color = yellow, write('Press Red').


%Wires

numOfColor([], _, 0).
numOfColor([H|T], H, N)	:- numOfColor(T, H, N1), N is N1+1.
numOfColor([H1|T], H, N)	:- H1 \= H, numOfColor(T, H, N).

lastDigitOdd(SerialNumber) :- length(SerialNumber, Length), Index is Length - 1, listIndex(SerialNumber, Index, Char), 1 is mod(Char,2).

solveWires(List, SerialNumber) :- length(List, 3), solveWires3(List).
solveWires(List, SerialNumber) :- length(List, 4), solveWires4(List, SerialNumber).
solveWires(List, SerialNumber) :- length(List, 5), solveWires5(List, SerialNumber).
solveWires(List, SerialNumber) :- length(List, 6), solveWires6(List, SerialNumber).

solveWires3(List) :- numOfColor(List, red, 0), write('Cut second wire').
solveWires3(List) :- listIndex(List, 2, white), write('Cut last wire').
solveWires3(List) :- numOfColor(List, blue, NumBlue), NumBlue > 1, write('Cut last blue wire').
solveWires3(List) :- write('Cut last wire').

solveWires4(List, SerialNumber) :- numOfColor(List, red, NumRed), NumRed > 1, lastDigitOdd(SerialNumber), write('Cut the last red wire').
solveWires4(List, SerialNumber) :- listIndex(List, 3, yellow), numOfColor(List, red, 0), write('Cut the first wire').
solveWires4(List, SerialNumber) :- numOfColor(List, blue, 1), write('Cut the first wire').
solveWires4(List, SerialNumber) :- numOfColor(List, yellow, NumYellow), NumYellow > 1, write('Cut the last wire').
solveWires4(List, SerialNumber) :- write('Cut the second wire').

solveWires5(List, SerialNumber) :- listIndex(List, 4, black), lastDigitOdd(SerialNumber), write('Cut the fourth wire').
solveWires5(List, SerialNumber) :- numOfColor(List, red, 1), numOfColor(List, yellow, NumYellow), NumYellow > 1, write('Cut the first wire').
solveWires5(List, SerialNumber) :- numOfColor(List, black, 0), write('Cut the second wire').
solveWires5(List, SerialNumber) :- write('Cut the first wire').

solveWires6(List, SerialNumber) :- numOfColor(List, yellow, 0), lastDigitOdd(SerialNumber), write('Cut the third wire').
solveWires6(List, SerialNumber) :- numOfColor(List, yellow, 1), numOfColor(List, white, NumWhite), NumWhite > 1, write('Cut the fourth wire').
solveWires6(List, SerialNumber) :- numOfColor(List, red, 0), write('Cut the last wire').
solveWires6(List, SerialNumber) :- write('Cut the fourth wire').

%Wire Sequences

solveWireSeq :- write('Keep track of how many wires of each color we have examined for a total of 3 numbers that start at 1 each'),
	     nl, write('Use handleSeq to start examining wires').

handleSeq(Color,Letter,R,B,K) :- Color = red, handleRed(Letter, R).
handleSeq(Color,Letter,R,B,K) :- Color = blue, handleBlue(Letter, B).
handleSeq(Color,Letter,R,B,K) :- Color = black, handleBlack(Letter, K).

handleRed(Letter, X) :- X is 1, Letter = c, write('Cut that wire').
handleRed(Letter, X) :- X is 2, Letter = b, write('Cut that wire').
handleRed(Letter, X) :- X is 3, Letter = a, write('Cut that wire').
handleRed(Letter, X) :- X is 4, Letter = a, write('Cut that wire').
handleRed(Letter, X) :- X is 4, Letter = c, write('Cut that wire').
handleRed(Letter, X) :- X is 5, Letter = b, write('Cut that wire').
handleRed(Letter, X) :- X is 6, Letter = a, write('Cut that wire').
handleRed(Letter, X) :- X is 6, Letter = c, write('Cut that wire').
handleRed(Letter, X) :- X is 7, Letter = a, write('Cut that wire').
handleRed(Letter, X) :- X is 7, Letter = b, write('Cut that wire').
handleRed(Letter, X) :- X is 7, Letter = c, write('Cut that wire').
handleRed(Letter, X) :- X is 8, Letter = a, write('Cut that wire').
handleRed(Letter, X) :- X is 8, Letter = b, write('Cut that wire').
handleRed(Letter, X) :- X is 9, Letter = b, write('Cut that wire').
handleRed(Letter, R) :- write('Skip that wire').

handleBlue(Letter, X) :- X is 1, Letter = b, write('Cut that wire').
handleBlue(Letter, X) :- X is 2, Letter = a, write('Cut that wire').
handleBlue(Letter, X) :- X is 2, Letter = c, write('Cut that wire').
handleBlue(Letter, X) :- X is 3, Letter = b, write('Cut that wire').
handleBlue(Letter, X) :- X is 4, Letter = a, write('Cut that wire').
handleBlue(Letter, X) :- X is 5, Letter = b, write('Cut that wire').
handleBlue(Letter, X) :- X is 6, Letter = b, write('Cut that wire').
handleBlue(Letter, X) :- X is 6, Letter = c, write('Cut that wire').
handleBlue(Letter, X) :- X is 7, Letter = c, write('Cut that wire').
handleBlue(Letter, X) :- X is 8, Letter = a, write('Cut that wire').
handleBlue(Letter, X) :- X is 8, Letter = c, write('Cut that wire').
handleBlue(Letter, X) :- X is 9, Letter = a, write('Cut that wire').
handleBlue(Letter, X) :- write('Skip that wire').

handleBlack(Letter, X) :- X is 1, Letter = a, write('Cut that wire').
handleBlack(Letter, X) :- X is 1, Letter = b, write('Cut that wire').
handleBlack(Letter, X) :- X is 1, Letter = c, write('Cut that wire').
handleBlack(Letter, X) :- X is 2, Letter = a, write('Cut that wire').
handleBlack(Letter, X) :- X is 2, Letter = c, write('Cut that wire').
handleBlack(Letter, X) :- X is 3, Letter = b, write('Cut that wire').
handleBlack(Letter, X) :- X is 4, Letter = a, write('Cut that wire').
handleBlack(Letter, X) :- X is 4, Letter = c, write('Cut that wire').
handleBlack(Letter, X) :- X is 5, Letter = b, write('Cut that wire').
handleBlack(Letter, X) :- X is 6, Letter = b, write('Cut that wire').
handleBlack(Letter, X) :- X is 6, Letter = c, write('Cut that wire').
handleBlack(Letter, X) :- X is 7, Letter = a, write('Cut that wire').
handleBlack(Letter, X) :- X is 7, Letter = b, write('Cut that wire').
handleBlack(Letter, X) :- X is 8, Letter = c, write('Cut that wire').
handleBlack(Letter, X) :- X is 9, Letter = c, write('Cut that wire').
handleBlack(Letter, X) :- write('Skip that wire').