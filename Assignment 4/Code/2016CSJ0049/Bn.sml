use "sig_bn.sml";
structure BIGNAT:B=

	struct 


	type bignat =(int list) 
	
	exception underflow
	exception division_by_zero
	exception emptyList
	exception overflow

	fun normalize(L)=
	if(L=[]) then
	raise emptyList	
	else if(L=[0]) then
	[0]
	else if (hd L)=0 then	
	normalize(tl L)
	else
	L	

	val zero=([0])	
	val one =[1]
	fun cmp(  (L1), (L2),i)=
	
	if(L1=[] andalso L2=[]) then
	EQUAL
	else if(i=1) then
	( if(length L1 > length L2) then
	  GREATER
	  else if(length L2 > length L1) then
	  LESS
	  else
	  cmp((L1),(L2),2)
	) 
	else
    (	
    	if (hd L1 > hd L2) then
    	GREATER
    	else if(hd L2 > hd L1) then
    	LESS
    	else
    	cmp((tl L1),(tl L2),2)	
	)


	fun compare((L1),(L2))=
	if(L1=[] orelse L2=[]) then
	raise emptyList
	else	
	cmp(normalize(L1),normalize(L2),1)

	fun toString(L)=
	let
			val i2s = Int.toString
		in
			if(L=[]) then
			""
			else 	
			""^(i2s (hd L))^""^toString(tl L)
		end


	

fun helpfromString(s)=
	if(s=[]) then
	[]
	else	
	(ord (hd s)-48)::helpfromString((tl s))	


fun fromString(s)=
	helpfromString(explode(s))



fun add(L1,L2,carry)=

	if (L1=[] andalso L2=[]) then
	[carry]
	else if(L2=[]) then
	( if(carry=0) then
	  L1
	 else	
	 add(L1,[carry],0)
	)
	else if(L1=[]) then
	( if(carry=0) then
	  L2
	  else
	  add(L2,[carry],0)
	)
	else
	( ((hd L1 + hd L2 + carry) mod 10)::add(tl L1,tl L2,((hd L1 + hd L2 + carry) div 10)) )	

fun ++(L1,L2)=
	if(L1=[] orelse L2=[]) then
	raise emptyList
	else	
	normalize( rev( add(rev L1,rev L2,0) ) )	

fun sub(L1,L2,carry)=

	if(L2=[]) then
	( if(L1=[]) then
	  []
	  else if(hd L1-carry >= 0) then
	  (hd L1-carry)::sub(tl L1,[],0)
	  else
	  (10+hd L1-carry)::sub(tl L1,[],1)
	)	
	else if(hd L1-carry>= hd L2) then
	(hd L1-hd L2-carry)::sub(tl L1,tl L2,0)
	else
	(10+hd L1-hd L2-carry)::sub(tl L1,tl L2,1)	

fun ~~(L1,L2)=
let 
		val a =cmp(normalize(L1),normalize(L2),1)
in
		if(a=GREATER) then
		normalize (rev (sub(rev (L1),rev(L2),0)) )
		else
		normalize( rev (sub(rev (L2),rev(L1),0)) )
end

fun --(L1,L2)=
let 
		val a =cmp(normalize(L1),normalize(L2),1)
in
	    if(L1=[] orelse L2=[]) then
	    raise emptyList	
		else if(a=GREATER orelse a=EQUAL) then
		normalize (rev (sub(rev (L1),rev(L2),0)) )
		else
		raise underflow
end







fun succ(L)=
	++(normalize(L),[1])	

fun <<(L1,L2)=
	let
		val n = compare(L1,L2)
	in
		if n=LESS then
		true
		else 
		false	
	end	

fun <<=(L1,L2)=
	let
		val n = compare(L1,L2)
	in
		if n=LESS orelse n=EQUAL then
		true
		else 
		false	
	end	

fun >>(L1,L2)=
    let
		val n = compare(L1,L2)
	in
		if n=GREATER then
		true
		else 
		false	
	end

fun >>=(L1,L2)=
	let
		val n = compare(L1,L2)
	in
		if n=GREATER orelse n=EQUAL then
		true
		else 
		false	
	end 

fun ==(L1,L2)=

	let 
		val n=compare(L1,L2)
	in
		if n=EQUAL then
		true
		else
		false	
	end		

fun len(L)=
	length( normalize(L) )

fun lenCompare(L1,L2)=
let	
	val l1=len(L1);
	val l2=len(L2);
in
	if(l1>l2) then
    GREATER
	else if(l1<l2) then
	LESS
	else
	EQUAL
end


fun lenLt(L1,L2)=
let
	val n = lenCompare(L1,L2)
in
	if(n=LESS) then
	true
	else
	false	
end


fun lenLeq(L1,L2)=
let
	val n = lenCompare(L1,L2)
in
	if(n=GREATER) then
	false
	else
	true	
end

fun lenGt(L1,L2)=
let
	val n = lenCompare(L1,L2);
in
	if(n=GREATER) then
	true
	else
	false

end

fun lenGeq(L1,L2)=
let
	val n = lenCompare(L1,L2);
in
	if(n=LESS) then
	false
	else
	true
		
end

fun lenEq(L1,L2)=
let
	val n = lenCompare(L1,L2);
in
	if(n=EQUAL) then
	true
	else
	false
		
end



fun pred(L)=
	if(L=[0]) then
	raise underflow
	else	
	~~(normalize(L),[1])

fun min(L1,L2)=
	let 
		val n=compare(L1,L2)
	in
		if(n=GREATER) then
		L2	
		else
		L1	
	end	

fun max(L1,L2)=
	let 
		val n=compare(L1,L2)
	in
		if(n=GREATER) then
		L1	
		else
		L2	
	end			



fun mulsingle(L,x,carry)=
if(L=[]) then
[carry]
else	
( ((hd L)*x+carry) mod 10 )::mulsingle(tl L,x,((hd L)*x+carry) div 10)  

fun mulsingleuse(L,x)=
	rev(mulsingle((rev L),x,0))

fun multiply(L1,L2)=

let
	val s = mulsingle(L1,hd L2,0)
in
	if(L2=[0]) then
	[0]	
	else	
	add(s,multiply(0::L1,tl L2),0)
end


fun **(L1,L2)=
	let
		val n = compare(L1,L2)
	in
		if n=GREATER then
		normalize( rev( multiply(rev L1,rev(0::L2)) ))
		else
		normalize( rev( multiply(rev L2,rev(0::L1)) ))		

	end	

fun extract (L,i,S)=
	if(i=0) then
	(rev S,L)
	else
	extract(tl L,i-1,hd L::S)


fun find(L1 , L2 , i )=
	let
		val v = --(  L2,**(L1,i)  )
	in
	if(  <<( v, L1 ) )then
	(v,i)	
    else
    find(L1 , L2 , ++(i,[1]) )

end
	
fun div2(L,L1,L2,r,q)=	

	if(L2=[]) then
	( normalize(q),r)
	else
	(
		let 
		val v=find(L,L1,[0])
		val a= #1(v)
		val b= #2(v)	
		in
			div2( L, a @ [hd L2] , tl L2 , a , q @ b )

		end	

	)

fun %%(L1,L2)=
	
	if(normalize(L1)=[0]) then
	raise division_by_zero
	else if(compare(L1,L2)=GREATER) then
		(zero,L2)
	else
	let
		val n = length(L1)
		val m = extract(L2,n,[])
		val first = #1 m
		val second = #2 m
	in
			
		div2(L1,first,second @ [0],[0],[0])


	end	

fun quo(L1,L2) = #1( %%(L1,L2) ) 	

fun rem(L1,L2) = #2( %%(L1,L2) ) 
end;    