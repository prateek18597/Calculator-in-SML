use "t.sml";
datatype expression =None | Bracket of expression |Digit of bigint | Add of expression*expression | Sub of expression*expression | Mul of expression*expression | Div of expression*expression; 

fun ignoreBracket([],L,l,r)=[]
	| ignoreBracket(h::t,L,l,r)=
		if h= #"(" then
		ignoreBracket(t,h::L,l+1,r)
		else
			if h= #")" andalso (l-1)=r then
				t
			else
				if h= #")" then
				ignoreBracket(t,h::L,l,r+1)
				else
				ignoreBracket(t,h::L,l,r)

fun ignoreBracketC([],i,l,r)=i
	| ignoreBracketC(h::t,i,l,r)=
		if h= #"(" then
		ignoreBracketC(t,i+1,l+1,r)
		else
			if h= #")" andalso (l-1)=r then
				i+1
			else
				if h= #")" then
				ignoreBracketC(t,i+1,l,r+1)
				else
				ignoreBracketC(t,i+1,l,r)

fun ignore(S)=
	ignoreBracket(String.explode(S),[],0,0);

fun findBracket([],L,i)= ~1
	| findBracket(h::t,L,i)=
		if h= #"(" then
			i
		else
			findBracket(t,L,i+1)

fun findDiv([],L,i)= findBracket(L,L,0)
	| findDiv(h::t,L,i)=
	if h= #"/" then
		i
	else
		if h= #"(" then
				findDiv((ignoreBracket(t,[],1,0)),L,(i+1+(ignoreBracketC(t,0,1,0))))
		else
		findDiv(t,L,i+1)

fun findMul([],L,i)= findDiv(L,L,0)
	| findMul(h::t,L,i)=
	if h= #"*" then
		i
	else
		if h= #"(" then
				findMul((ignoreBracket(t,[],1,0)),L,(i+1+(ignoreBracketC(t,0,1,0))))
		else
			findMul(t,L,i+1)

fun findSub([],L,i,c)= findMul(L,L,0)
	| findSub(h::t,L,i,c)=
	if h= #"-" andalso (c= #"*" orelse c= #"/") then
		i-1
	else
		if h= #"-" then
			i
		else
			if h= #"(" then
				findSub((ignoreBracket(t,[],1,0)),L,(i+1+(ignoreBracketC(t,0,1,0))),h)
			else
				findSub(t,L,i+1,h)

fun findAdd([],L,i,c)= findSub(L,L,0, #" ")
	| findAdd(h::t,L,i,c)=
	if h= #"+" andalso (c= #"*" orelse c= #"/") then
		i-1
	else
		if h= #"+" then
			i
		else
			if h= #"(" then
				findAdd((ignoreBracket(t,[],1,0)),L,(i+1+(ignoreBracketC(t,0,1,0))),h)
			else
				findAdd(t,L,i+1,h)

fun find(S)=
	findAdd(String.explode(S),String.explode(S),0, #" ")

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
							if find(S)>=0 andalso List.nth(String.explode(S),find(S))= #"(" then
								Bracket(input(String.substring(S,(find(S)+1),(ignoreBracketC(List.tl(String.explode(S)),0,1,0))-1)))
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
	| solve(Bracket(X))=
		solve(X)

fun calculate("")= raise empty_expression
	| calculate(S)=
	toString(solve(input(S)))

fun removeNewline("")=""
	|	removeNewline(S)=
	String.substring(S,0,size(S)-1);

fun calculator()=
	(
		print (calculate(removeNewline(Option.valOf(TextIO.inputLine(TextIO.stdIn))))^"\n");
		if (Option.valOf(TextIO.inputLine(TextIO.stdIn)))<>"quit\n" then
			calculator()
		else
			"Good Bye."
	)
