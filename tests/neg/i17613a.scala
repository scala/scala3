//> using options -Xfatal-warnings -Xlint:type-parameter-shadow

object i17613a:
	class B:
		type T = Int
		trait D

		def foobar[D](in: D) = in.toString // error method parameter shadows some other type
		type MySeq[D] = Seq[D] // error type member's parameter shadows some other type

		class Foo[T](t: T): // error class parameter shadows some other type
			def bar[T](w: T) = w.toString // error a type parameter shadows another type parameter

		// even deeply nested...
		class C[M[List[_]]] // error
		type E[M[List[_]]] = Int // error
		def foo[N[M[List[_]]]] = ??? // error

		// ...but not between type parameters in the same list
		class F[A, M[N[A]]]
		type G[A, M[L[A]]] = Int
		def bar[A, N[M[L[A]]]] = ???
	def main(args: Array[String]) = println("Test for type parameter shadow")
