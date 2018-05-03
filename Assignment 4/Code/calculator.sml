use "t.sml";

datatype expression =None |Digit of BigInt | Add of expression*expression | Sub of expression*expression | Mul of expression*expression | Div of expression*expression; 

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

fun getFirst([],L)= Digit (valOf(Int.fromString(String.implode(List.rev(L)))))
		| getFirst(h::t,L)=
			if(Char.isDigit(h)) then
				getFirst(t,h::L)
			else
				Digit (valOf(Int.fromString(String.implode(List.rev(L)))))

	fun getT([])= []
		| getT(h::[])=[]
		| getT(h::t)=
			if(Char.isDigit(h)) then
				getT(t)
			else
				h::t

fun applygetT(s)=
	getT(String.explode(s));
fun applyFirst(s)=
	getFirst(String.explode(s),[]);
local
	


	fun check([],L,R,0)= Add(R,Digit (valOf(Int.fromString(String.implode(List.rev(L))))))
	|
		check([],L,R,1)= Add(R,Digit (valOf(Int.fromString(String.implode(List.rev(L))))))
	| check([],L,R,2)= Mul(R,Digit (valOf(Int.fromString(String.implode(List.rev(L))))))
	|check([],L,R,3)= Sub(R,Digit (valOf(Int.fromString(String.implode(List.rev(L))))))
	|
		check(h::t,L,R,O)=
			if(Char.isDigit(h)) then
				check(t,h::L,R,O)
			else
				if h= #"+" then
					Add(Digit (valOf(Int.fromString(String.implode(List.rev(L))))),check(t,[],None,1))
					(*check(t,[],Add(R,Digit (valOf(Int.fromString(String.implode(List.rev(L)))))),1)*)
				else
					if h= #"*" then
						(*check(t,[],Add(R,(Mul(getFirst(String.explode(t),[]),Digit (valOf(Int.fromString(String.implode(List.rev(L)))))))),1)*)
						Mul(Digit (valOf(Int.fromString(String.implode(List.rev(L))))),check(t,[],None,2))
					else
						if h= #"-" then
						Sub(Digit (valOf(Int.fromString(String.implode(List.rev(L))))),check(t,[],None,3))
					else
						R
					(*check(t,[],Add(Digit (valOf(Int.fromString(String.implode(List.rev(L))))),check(t,[],R)))*)
in	
	fun handleInput s=
		(*if List.hd(getT(String.explode(s)))= #"+" then
		check(getT(String.explode(s)),getFirst(String.explode(s),[]),None,1)
		else
			if List.hd(getT(String.explode(s)))= #"*" then
		check(getT(String.explode(s)),getFirst(String.explode(s),[]),None,2)
		else
			if List.hd(getT(String.explode(s)))= #"-" then
		check(getT(String.explode(s)),getFirst(String.explode(s),[]),None,3)
		else*)
		check(String.explode(s),[],None,0)
end

fun findDiv([],L,i)= ~1
	| findDiv(h::t,L,i)=
	if h= #"/" then
		i
	else
		findDiv(t,L,i+1)


fun findMul([],L,i)= findDiv(L,L,0)
	| findMul(h::t,L,i)=
	if h= #"*" then
		i
	else
		findMul(t,L,i+1)


fun findSub([],L,i)= findMul(L,L,0)
	| findSub(h::t,L,i)=
	if h= #"-" then
		i
	else
		findSub(t,L,i+1)


fun findAdd([],L,i)= findSub(L,L,0)
	| findAdd(h::t,L,i)=
	if h= #"+" then
		i
	else
		findAdd(t,L,i+1)

fun find(S)=
	findAdd(String.explode(S),String.explode(S),0)

fun part1(h::t,L2,i,j)=
	if i=j then
		String.implode(t)
	else
		part1(t,h::L2,i+1,j)

fun part2(h::t,L2,i,j)=
	if i=j then
		String.implode(List.rev(L2))
	else
		part2(t,h::L2,i+1,j)

fun changeptom([],L)=List.rev(L)
| changeptom(h::t,L)=
	if h= #"-" then
		changeptom(t, #"+"::L)
	else
		changeptom(t,h::L)


fun breakDown(S)=
	if find(S)>=1 andalso List.nth(String.explode(S),find(S))= #"+" then
		Add(breakDown(part1(String.explode(S),[],0,find(S))),breakDown(part2(String.explode(S),[],0,find(S))))
	else
		if find(S)>=1 andalso List.nth(String.explode(S),find(S))= #"-" then
			Sub(breakDown(part1(changeptom(String.explode(S),[]),[],0,find(S))),breakDown(part2(String.explode(S),[],0,find(S))))
		else
			if find(S)>=1 andalso List.nth(String.explode(S),find(S))= #"*" then
				Mul(breakDown(part1(String.explode(S),[],0,find(S))),breakDown(part2(String.explode(S),[],0,find(S))))
			else
				if find(S)>=1 andalso List.nth(String.explode(S),find(S))= #"/" then
					Div(breakDown(part1(String.explode(S),[],0,find(S))),breakDown(part2(String.explode(S),[],0,find(S))))
				else
					Digit(valOf(Int.fromString(S)))

fun evaluate(None)= 0
	| evaluate(Digit(B))= B
	| evaluate(Add(X,A)) =
	if X<>None andalso A<>None then
	evaluate(A)+evaluate(X)
	else
		if X<>None then
			evaluate(X)
		else
			if A<>None then
				evaluate(A)
			else
				0
	| evaluate(Mul(X,A))=
	if X<>None andalso A<>None then
	evaluate(A)*evaluate(X)
	else
		if X<>None then
			evaluate(X)
		else
			if A<>None then
				evaluate(A)
			else
				0
	| evaluate(Sub(X,A))=
	if X<>None andalso A<>None then
		evaluate(A)-evaluate(X)
	else
		if X<>None then
			0-evaluate(X)
		else
			if A<>None then
				evaluate(A)
			else
				0
	| evaluate(Div(X,A))=
	if X<>None andalso A<>None then
		evaluate(A) div evaluate(X)
	else
		0
