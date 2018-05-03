use "Bn.sml";

functor BigInt (Bn: B) :
 
  sig
  datatype bigint= Pair of ((Bn.bignat) * (bool));
  
  val bigint : int -> bigint
  val int : bigint -> int option

  val compare : bigint * bigint -> order
  val toString: bigint -> string
  val fromString : string -> bigint
  val << : bigint * bigint -> bool
  val <<= : bigint * bigint -> bool
  val >> : bigint * bigint -> bool
  val >>= : bigint * bigint -> bool
  val == : bigint * bigint -> bool
  val ~~ : bigint -> bigint	
  val abs : bigint -> bigint
  val len: bigint->int
  val bigzero: bigint
  val lenCompare : bigint * bigint -> order
  val ** : bigint * bigint -> bigint
  val lenLt : bigint * bigint -> bool
  val lenLeq : bigint * bigint -> bool
  val lenGt : bigint * bigint -> bool
  val lenGeq : bigint * bigint -> bool
  val lenEq : bigint * bigint -> bool
  val normalize : bigint -> bigint
  val min : bigint * bigint -> bigint
  val max : bigint * bigint -> bigint
  val sign : bigint -> string
  val sameSign : bigint * bigint -> bool
  val ++ : bigint * bigint -> bigint 
  val -- : bigint * bigint -> bigint
  
  exception division_by_zero

  val %% : bigint * bigint -> bigint * bigint
  
  val quo : bigint * bigint -> bigint
  val rem : bigint * bigint -> bigint
  
  val pred : bigint -> bigint
  val succ : bigint -> bigint

  
  end	
  =
struct
	
	exception division_by_zero
	datatype bigint = Pair of ((Bn.bignat) * (bool))
	;
	fun toString(Pair(L,true))=Bn.toString(L)
		| toString(Pair(L,false))="-"^Bn.toString(L)

	fun fromString(s)=
		let
			val a = explode s
			val l= length a;
		in
		   if (ord (hd a)=126) then
		   Pair(Bn.fromString(substring(s,1,l-1)),false)
		   else
		   Pair(Bn.fromString(s),true)	   	
		end	

	val bigzero=Pair(Bn.zero,true)	


	fun compare(Pair(L1,true),Pair(L2,true))=Bn.compare(L1,L2)
	  | compare(Pair(L1,true),Pair(L2,false))=GREATER	
	  |	compare(Pair(L1,false),Pair(L2,true))=LESS
	  | compare(Pair(L1,false),Pair(L2,false))=
	  	if Bn.compare(L1,L2)=GREATER then
	  	LESS
	  	else if Bn.compare(L1,L2)=LESS then
	  	GREATER
	  	else
	  	EQUAL 	

	fun <<( Pair(L1,x),Pair(L2,y) )=
	if(compare(Pair(L1,x),Pair(L2,y))=LESS) then
	true
	else
	false 


	fun <<=( Pair(L1,x),Pair(L2,y) )=
	if(compare(Pair(L1,x),Pair(L2,y))=LESS orelse compare(Pair(L1,x),Pair(L2,y))=EQUAL) then	
	true
	else 
	false	

	fun >>( Pair(L1,x),Pair(L2,y) )=
	if(compare(Pair(L1,x),Pair(L2,y))=GREATER) then
	true
	else
	false	

	fun >>=( Pair(L1,x),Pair(L2,y) )=
	if(compare(Pair(L1,x),Pair(L2,y))=GREATER orelse compare(Pair(L1,x),Pair(L2,y))=EQUAL ) then
	true
	else
	false

	
	fun == (Pair(L1,x),Pair(L2,y))=	
	if(compare(Pair(L1,x),Pair(L2,y))=EQUAL ) then
	true
	else
	false	

	fun ~~(Pair(L,x))=Pair(L,(not) x)
	
	fun abs(Pair(L,_))=Pair(L,true)

    

    fun len(Pair(L,x))=Bn.len(L)
	
    fun lenCompare(Pair(L1,_),Pair(L2,_))=Bn.lenCompare(L1,L2)	

    	
  


  fun lenLt(Pair(L1,x),Pair(L2,y))=
  	if(lenCompare(Pair(L1,x),Pair(L2,y))=LESS) then
  	true
    else
    false
 
   fun lenLeq(Pair(L1,x),Pair(L2,y))=
   if(lenCompare(Pair(L1,x),Pair(L2,y))=GREATER) then
   false
   else
   true

   fun lenGt(Pair(L1,x),Pair(L2,y))=
   if(lenCompare(Pair(L1,x),Pair(L2,y))=GREATER) then
   true
   else
   false


   fun lenGeq(Pair(L1,x),Pair(L2,y))=
   if(lenCompare(Pair(L1,x),Pair(L2,y))=LESS) then
   false
   else
   true

   fun lenEq(Pair(L1,x),Pair(L2,y))=
   if(lenCompare(Pair(L1,x),Pair(L2,y))=EQUAL) then
   true
   else
   false;


   fun normalize(Pair(L,x))=Pair(Bn.normalize(L),x);

   fun min(Pair(L1,x),Pair(L2,y))=
   if(compare(Pair(L1,x),Pair(L2,y))=GREATER) then
   Pair(L2,y)
   else
   Pair(L1,x)


   fun max(Pair(L1,x),Pair(L2,y))=
   if(compare(Pair(L1,x),Pair(L2,y))=GREATER) then
   Pair(L1,y)
   else
   Pair(L2,x)	

	 	

   fun sign(Pair(L,x))=
   if(x) then
   "+"
   else
   "-"

   fun sameSign(Pair(L1,x),Pair(L2,y))=
   if(x=y) then
   true
   else
   false

   fun ++(Pair(L1,true),Pair(L2,true))=Pair(Bn.++(L1,L2),true)
	 |	++(Pair(L1,false),Pair(L2,false))=Pair(Bn.++(L1,L2),false)
     
     |  ++(Pair(L1,true),Pair(L2,false))=
     	
     	if(Bn.compare(L1,L2)=GREATER) then
        Pair(Bn.~~(L1,L2),true)
        
        else if(Bn.compare(L1,L2)=LESS) then
        Pair(Bn.~~(L1,L2),false)
        
        else
        Pair(Bn.zero,true)	
     
     |  ++(Pair(L1,false),Pair(L2,true))=
        if(Bn.compare(L1,L2)=LESS) then
        Pair(Bn.~~(L1,L2),true)
        else if(Bn.compare(L1,L2)=GREATER) then
        Pair(Bn.~~(L1,L2),false)
        else
        Pair(Bn.zero,true)	


     fun --(Pair(L1,true),Pair(L2,false))=Pair(Bn.++(L1,L2),true)
     	| --(Pair(L1,false),Pair(L2,true))=Pair(Bn.++(L1,L2),false)
     	| --(Pair(L1,true),Pair(L2,true))=
     	  if(Bn.compare(L1,L2)= GREATER) then
     	  Pair(Bn.~~(L1,L2),true)	
     	  else if(Bn.compare(L1,L2)= LESS) then
     	  Pair(Bn.~~(L1,L2),false) 
     	  else 
		  Pair(Bn.zero,true)
		| --(Pair(L1,false),Pair(L2,false))=
		    if(Bn.compare(L1,L2)=LESS) then
		   	Pair(Bn.~~(L1,L2),true)
		    else if(Bn.compare(L1,L2)=GREATER) then
		   	Pair(Bn.~~(L1,L2),false)
		    else
		   	Pair(Bn.zero,true)    

		fun succ(Pair(L1,x))= ++ ( Pair(L1,x), Pair(Bn.one,true))
		
		fun pred((Pair(L1,x)))= ++ ( (Pair(L1,x)) , (Pair(Bn.one,false)) )	


		fun **( Pair(L1,true), Pair(L2,true) ) = Pair( Bn.**(L1,L2),true )
		  | **( Pair(L1,false), Pair(L2,false) ) = Pair(Bn.**(L1,L2),true)  	
		  | **( Pair(L1,true), Pair(L2,false) ) = Pair(Bn.**(L1,L2),false)
		  | **( Pair(L1,false), Pair(L2,true) ) = Pair(Bn.**(L1,L2),false)


		fun %%( Pair(L1,true), Pair(L2,true) )  = ( Pair(#1 ( Bn.%%(L1,L2)  ),true ) , Pair( #2 ( Bn.%%(L1,L2)),true ) )  
         |   %%(Pair(L1,false), Pair(L2,true)) = ( Pair(#1 ( Bn.%%(L1,L2) ), false ) , Pair(#2 ( Bn.%%(L1,L2) ),true ) )
         
         |   %%(Pair(L1,true), Pair(L2,false)) =
         		let
         			val a = #1 (Bn.%%(L1,L2))
         			val b = #2 (Bn.%%(L1,L2))
                    val c = Bn.++(a,Bn.one)
                    val d = Bn.--(L1,b)
         		in
         			if( Bn.==( Bn.normalize(b),Bn.zero ) ) then
         			(Pair(a,false),Pair(b,true))
         			else
         			(Pair(c,false),Pair(d,true))	

         		end

        |   %%(Pair(L1,false), Pair(L2,false)) =
         		let
         			val a = #1 (Bn.%%(L1,L2))
         			val b = #2 (Bn.%%(L1,L2))
                    val c = Bn.++(a,Bn.one)
                    val d = Bn.--(L1,b)
         		in
         			if( Bn.==( Bn.normalize(b),Bn.zero ) ) then
         			(Pair(a,true),Pair(b,true))
         			else
         			(Pair(c,true),Pair(d,true))	

         		end 		

        fun quo(Pair(L1,x),Pair(L2,y))= #1(%%(Pair(L1,x),Pair(L2,y)))
        
        fun rem ((Pair(L1,x),Pair(L2,y)))= #2 (%%(Pair(L1,x),Pair(L2,y)))

	
	fun bigint(a)=
	fromString(Int.toString(a))	

	fun int(P)=
		Int.fromString(toString(P))


end

structure name = BigInt(BIGNAT); 

open name;