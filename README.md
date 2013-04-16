A macro-based unified Tuple class.

Encodes Tuples as Arrays

Build with SBT

What Works:
 * Tuple Creation
 * Getting Tuple Members
 * Stringification
 * (Structural) Equality
What Doesn't Work (exist):
 * Pattern Matching
 * Conversion to TupleN
 * Copy Method

Example:

	import tuplicity.{Tuple=>T, _}
	
	val tup = T(1,2,3, "a")
	
	println(tup._2) // prints 2
	
	val x: String = tup._4

