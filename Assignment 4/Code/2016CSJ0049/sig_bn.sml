signature B = 

sig
	type bignat
	exception division_by_zero
	exception emptyList
	exception overflow
	val compare: bignat * bignat -> order
	val toString: bignat->string
	val fromString: string->bignat
	val zero: bignat 
	val len: bignat->int
	val ++ : bignat * bignat -> bignat
	
	val ~~ : bignat * bignat -> bignat
	val one : bignat
	
	val succ: bignat -> bignat
	val << : bignat * bignat -> bool
	val <<= : bignat * bignat -> bool
	val >> : bignat * bignat -> bool
	val >>= : bignat * bignat -> bool
	val == : bignat * bignat -> bool
	val lenCompare : bignat * bignat -> order
	val lenLt : bignat * bignat -> bool
    val lenLeq : bignat * bignat -> bool
    val lenGt : bignat * bignat -> bool
    val lenGeq : bignat * bignat -> bool
    val lenEq : bignat * bignat -> bool
    val normalize : bignat -> bignat
    val -- : bignat * bignat -> bignat
    val min: bignat * bignat -> bignat
    val max: bignat * bignat -> bignat
    val ** : bignat * bignat -> bignat
    val pred : bignat -> bignat
    
    val %% : bignat * bignat -> bignat * bignat
    val quo : bignat * bignat -> bignat 
    val rem : bignat * bignat -> bignat
    exception underflow
end
