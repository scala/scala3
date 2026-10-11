//> using options -language:experimental.inlineTraits
inline trait A: // At the moment this works with an ordinary trait but throws a TypeError with inline traits 
	sealed class InnerA: // error: Inline traits may not define inner classes or traits.
		val x = new InnerB

	sealed class InnerB: // error: Inline traits may not define inner classes or traits.
		val x = 10

class B extends A:
	val y = 10
