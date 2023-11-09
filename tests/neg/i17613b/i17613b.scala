//> using options -Xlint:type-parameter-shadow -Xfatal-warnings

object i17613b:
	import importTry._
	class B:
		type T = Int
		trait Typeclass[T]
		trait D

		def foobar[ImTrait](in: D) = in.toString // warn
		type MySeq[ImTrait] = Seq[D] // warn

		def foobar2[ImClass](in: D) = in.toString // warn
		type MySeq2[ImClass] = Seq[D] // warn

		given [A]: Typeclass[Int]() // warn
		type TypeLambda[A] = [ImTrait] =>> Map[ImTrait, B]
		type PolyFun[A] = [ImTrait] => ImTrait => B // warn
		type MatchType[A] = A match {
			case String => Int
			case ImTrait => Boolean
		}

		class Foo[T](t: T): // warn class parameter shadows some other type
			import scala.collection.immutable.{List => List1}
			def bar[List](w: T) = w.toString // no warning due to the explicit import renaming

			def intType[List1](x: T) = x.toString() // warn

			type Y[List] = Int // no warning

			given [A]: Typeclass[A]()
			given [Int]: Typeclass[Int]() // warn

		class C[M[List[_]]] // warn List not renamed here
		type E[M[Int[_]]] = Int // warn

		def foo[N[M[List[_]]]] = // warn
			import importTry.{ImClass => ImClassR}
			def inner[ImClass] = // no warning
				type Z[ImClassR] = Int // warn
				class InnerCl[ImClassR] // warn
				5

	def main(args: Array[String]) = println("Test for type parameter shadow")

	// nopos-error fatal warnings
