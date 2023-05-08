// scalac: -Xlint:type-parameter-shadow

object i17613b:
	import importTry._
	class B:
		type T = Int
		trait D

		def foobar[ImTrait](in: D) = in.toString // error
		type MySeq[ImTrait] = Seq[D] // error

		def foobar2[ImClass](in: D) = in.toString // error
		type MySeq2[ImClass] = Seq[D] // error

		class Foo[T](t: T): // error class parameter shadows some other type
			import scala.collection.immutable.{List => List1}
			def bar[List](w: T) = w.toString // no warning due to the explicit import renaming

			def intType[List1](x: T) = x.toString() // error

			type Y[List] = Int // no warning

		class C[M[List[_]]] // error List not renamed here
		type E[M[Int[_]]] = Int // error

		def foo[N[M[List[_]]]] = // error
			import importTry.{ImClass => ImClassR}
			def inner[ImClass] = // no warning
				type Z[ImClassR] = Int // error
				class InnerCl[ImClassR] // error
				5

	def main(args: Array[String]) = println("Test for type parameter shadow")
