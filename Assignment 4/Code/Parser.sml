(*
Addition 1
Sub 2
Mul 3
Div 4
*)
local
	fun chartointlist([],L)=L
		| chartointlist (h::t,L) =
			chartointlist(t,L@[valOf(Int.fromString(String.str(h)))])
in
	fun fromString S =
		chartointlist(String.explode(S),[]);
end

fun getNum(h::t)=


local
	fun calculate([],X,L,O,S)=X
		| calculate(h::t,l,L,operator,S)=
		
		if(Char.isDigit(h)) then
				calculate(t,l,h::L,operator,S)
			else
				if h= #"+" then
					calculate(t,valOf(Int.fromString(String.implode(List.rev(L)))))
					(*Add(Digit (valOf(Int.fromString(String.implode(List.rev(L))))),check(t,[],Digit(0)))*)
				else
					if h= #"*" then
						
						Mul(Digit (valOf(Int.fromString(String.implode(List.rev(L))))),check(t,[],Digit(0)))
					else
						if h= #"-" then
						Sub(Digit (valOf(Int.fromString(String.implode(List.rev(L))))),check(t,[],Digit(0)))
					else
						R
						

in
fun Evaluate(S)=
	calculate(String.explode(S),lastvalue,[],operator)

end