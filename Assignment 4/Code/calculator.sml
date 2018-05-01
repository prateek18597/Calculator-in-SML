
datatype expression =Digit of int | Add of expression*expression | Sub of expression*expression | Mul of expression*expression | Div of expression*expression; 

infix ++;

fun fromInt x =
	Digit(x);	

fun makeAdd(x,y)=
	Add(x,y);

local
	fun chartointlist([],L)=L
		| chartointlist (h::t,L) =
			chartointlist(t,L@[valOf(Int.fromString(String.str(h)))])
in
	fun fromString S =
		chartointlist(String.explode(S),[]);
end

local
	fun check([],L,R)= Add(Digit (valOf(Int.fromString(String.implode(List.rev(L))))),Digit(0))
	|	check(h::t,L,R)=
			if(Char.isDigit(h)) then
				check(t,h::L,R)
			else
				if h= #"+" then
					Add(Digit (valOf(Int.fromString(String.implode(List.rev(L))))),check(t,[],Digit(0)))
				else
					if h= #"*" then
						Mul(Digit (valOf(Int.fromString(String.implode(List.rev(L))))),check(t,[],Digit(0)))
					else
						if h= #"-" then
						Sub(Digit (valOf(Int.fromString(String.implode(List.rev(L))))),check(t,[],Digit(0)))
					else
						R
					(*check(t,[],Add(Digit (valOf(Int.fromString(String.implode(List.rev(L))))),check(t,[],R)))*)
in	
	fun handleInput s=
		check(String.explode(s),[],Digit(0));
end



fun evaluate(Digit(B))= B
	| evaluate(Add(Digit(A),X)) =
	A+evaluate(X)
	| evaluate(Mul(Digit(A),X))=
	evaluate(X)*A
	| evaluate(Sub(Digit(A),X))=
	A-evaluate(X)
