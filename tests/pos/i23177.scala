class Something

transparent inline def test(inline oNio: Something | Null = null, inline offset: Int = 0): Int = {
	inline val index = inline oNio match {
		case null => offset
		case nio: Something => -1
	}
	index
}

@main def check(): Unit = {
	test(Something(), 0) // compiles
	test(Something()) // compiles
	test(null, 0) // compiles
	test(null) // complains with: inline value must have a literal constant type 
}
