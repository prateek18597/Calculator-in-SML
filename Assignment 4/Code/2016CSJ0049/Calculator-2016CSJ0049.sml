datatype expression =None 
	| Bracket of expression 
	| Digit of bigint 
	| Add of expression*expression 
	| Sub of expression*expression 
	| Mul of expression*expression 
	| Div of expression*expression
	| Equal of expression*expression
	| Lessthan of expression*expression
	| LessthanEqual of expression*expression
	| Greaterthan of expression*expression
	| GreaterthanEqual of expression*expression
	
exception illegal_character;
local
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

fun findBracket([],L,i)= ~1
	| findBracket(h::t,L,i)=
		if h= #"(" then
			i
		else
			findBracket(t,L,i+1)

fun findDiv([],L,i)= findBracket(L,L,0)
	| findDiv(h::t,L,i)=
	
		if h= #"/" andalso ( hd(t)= #"*" orelse hd(t)= #"/" ) then
			~3
		else
			if h= #"/" then
				i
			else
				if h= #"(" then
					findDiv((ignoreBracket(t,[],1,0)),L,(i+1+(ignoreBracketC(t,0,1,0))))
				else
					findDiv(t,L,i+1)

fun findMul([],L,i)= findDiv(L,L,0)
	| findMul(h::t,L,i)=
	if h= #"*"  andalso ( hd(t)= #"*" orelse hd(t)= #"/" ) then
		~3
	else
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
				if h= #"-" orelse h= #"*" orelse h= #"+" orelse h= #"=" orelse h= #"/" orelse h= #"(" orelse h= #")" orelse Char.isDigit(h) then 
					findAdd(t,L,i+1,h)
				else
					~2



fun findEqual([],L,i,c)=findAdd(L,L,0, #" ")
	| findEqual(h::t,L,i,c)=
		if h= #"=" andalso hd(t)= #"=" then
			i
		else
			if h= #"-"  orelse h= #"<" orelse h= #">" orelse h= #"*" orelse h= #"+" orelse h= #"=" orelse h= #"/" orelse h= #"(" orelse h= #")" orelse Char.isDigit(h) then 
					findEqual(t,L,i+1,h)
				else
					~2

fun findLessEqual([],L,i,c)=findEqual(L,L,0, #" ")
	| findLessEqual(h::t,L,i,c)=
		if h= #"<" andalso hd(t)= #"<" then
			i
		else
			if h= #"-"  orelse h= #"<" orelse h= #">" orelse h= #"*" orelse h= #"+" orelse h= #"=" orelse h= #"/" orelse h= #"(" orelse h= #")" orelse Char.isDigit(h) then 
					findLessEqual(t,L,i+1,h)
				else
					~2

fun findGreaterEqual([],L,i,c)=findLessEqual(L,L,0, #" ")
	| findGreaterEqual(h::t,L,i,c)=
		if h= #">" andalso hd(t)= #">" then
			i
		else
			if h= #"-" orelse h= #"*" orelse h= #"," orelse h= #"<" orelse h= #">" orelse h= #"+" orelse h= #"=" orelse h= #"/" orelse h= #"(" orelse h= #")" orelse Char.isDigit(h) then 
					findGreaterEqual(t,L,i+1,h)
				else
					~2
fun find(S)=
	findGreaterEqual(String.explode(S),String.explode(S),0, #" ")

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
					if find(S)>=1 andalso List.nth(String.explode(S),find(S))= #"=" then
						Equal(input(part1(String.explode(S),[],0,find(S)+1)),input(part2(String.explode(S),[],0,find(S))))
					else
					if find(S)>=1 andalso List.nth(String.explode(S),find(S))= #"<" andalso List.nth(String.explode(S),find(S)+1)= #"<" andalso List.nth(String.explode(S),find(S))<> #"=" then
						Lessthan(input(part1(String.explode(S),[],0,find(S)+1)),input(part2(String.explode(S),[],0,find(S))))
					else
					if find(S)>=1 andalso List.nth(String.explode(S),find(S))= #"<" andalso List.nth(String.explode(S),find(S)+1)= #"<" andalso List.nth(String.explode(S),find(S))= #"=" then
						LessthanEqual(input(part1(String.explode(S),[],0,find(S)+2)),input(part2(String.explode(S),[],0,find(S))))
					else
					if find(S)>=1 andalso List.nth(String.explode(S),find(S))= #">" andalso List.nth(String.explode(S),find(S)+1)= #">" andalso List.nth(String.explode(S),find(S))<> #"=" then
						Greaterthan(input(part1(String.explode(S),[],0,find(S)+1)),input(part2(String.explode(S),[],0,find(S))))
					else
					if find(S)>=1 andalso List.nth(String.explode(S),find(S))= #">" andalso List.nth(String.explode(S),find(S)+1)= #">" andalso List.nth(String.explode(S),find(S))= #"=" then
						GreaterthanEqual(input(part1(String.explode(S),[],0,find(S)+2)),input(part2(String.explode(S),[],0,find(S))))
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
								if find(S)= ~2 then
									(	print "Illegel Character found\n";
										None
									)
								else
									if find(S)= ~3 then
									(	print "Syntax Error\n";
										None
									)
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
	| solve(Equal(X,Y))=
	 if	==(solve(X),solve(Y)) then
	 	fromString("1")
	 else
	 	bigzero
	| solve(LessthanEqual(X,Y))=
	 if	<<=(solve(X),solve(Y)) then
	 	fromString("1")
	 else
	 	bigzero
	| solve(Lessthan(X,Y))=
	 if	<<(solve(X),solve(Y)) then
	 	bigzero
	 else
	 	fromString("1")
	| solve(Greaterthan(X,Y))=
	 if	>>(solve(X),solve(Y)) then
	 	bigzero
	 else
	 	fromString("1")
	| solve(GreaterthanEqual(X,Y))=
	 if	>>=(solve(X),solve(Y)) then
	 	bigzero
	 else
	 	fromString("1")
    
fun calculate("")= "Empty expression found."
	| calculate(S)=
	toString(solve(input(S)))


fun removeNewline("")=""
	|	removeNewline(S)=
	String.substring(S,0,size(S)-1);

in

fun calculator()=
	(
		print ("->");
		print ("="^calculate(removeNewline(Option.valOf(TextIO.inputLine(TextIO.stdIn))))^"\nTo exit enter 'quit' and to continue Press Enter.\n");
		if (Option.valOf(TextIO.inputLine(TextIO.stdIn)))<>"quit\n" then
			calculator()
		else
			"Good Bye"
	)
end