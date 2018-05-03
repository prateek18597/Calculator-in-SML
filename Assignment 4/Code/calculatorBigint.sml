use "t.sml";

datatype expression =None |Digit of bigint | Add of expression*expression | Sub of expression*expression | Mul of expression*expression | Div of expression*expression; 

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

fun changedtom([],L)=List.rev(L)
| changedtom(h::t,L)=
	if h= #"/" then
		changedtom(t, #"*"::L)
	else
		changedtom(t,h::L)

fun input(S)=
	if find(S)>=1 andalso List.nth(String.explode(S),find(S))= #"+" then
		Add(input(part1(String.explode(S),[],0,find(S))),input(part2(String.explode(S),[],0,find(S))))
	else
		if find(S)>=1 andalso List.nth(String.explode(S),find(S))= #"-" then
			Sub(input(part1(changeptom(String.explode(S),[]),[],0,find(S))),input(part2(String.explode(S),[],0,find(S))))
		else
			if find(S)>=1 andalso List.nth(String.explode(S),find(S))= #"*" then
				Mul(input(part1(String.explode(S),[],0,find(S))),input(part2(String.explode(S),[],0,find(S))))
			else
				if find(S)>=1 andalso List.nth(String.explode(S),find(S))= #"/" then
					Div(input(part1(changedtom(String.explode(S),[]),[],0,find(S))),input(part2(String.explode(S),[],0,find(S))))
				else
					if find(S)=0 andalso List.nth(String.explode(S),find(S))= #"-" then
						Sub(input(String.implode(changeptom(String.explode(String.substring(S,1,String.size(S)-1)),[]))),Digit(bigzero))
					else
						if find(S)=0 andalso List.nth(String.explode(S),find(S))= #"+" then
							input(String.substring(S,1,String.size(S)-1))
						else
							Digit(fromString(S))

exception empty_expression;

fun solve(None)= bigzero
	| solve(Digit(B))= B
	| solve(Add(X,A)) =
		++(solve(A),solve(X))
	| solve(Mul(X,A))=
		**(solve(A),solve(X))
	| solve(Sub(X,A))=
		--(solve(A),solve(X))
	| solve(Div(X,A))=
		quo(solve(X),solve(A))
fun calculate("")= raise empty_expression
	| calculate(S)=
	toString(solve(input(S)))