//> using options  -Xlint:private-shadow -source:3.3

object i17612a:
	class Base(var x: Int, val y: Int, var z: Int):
		var shadowed2 = 2
		val shadowed3 = 3
		val shadowed4 = 4
		protected var shadowed5 = 5
		//var shadowed6 = 6

		val notShadowed = -1
		private val notShadowed2 = -2
		//val fatalOverride = 0

		def increment(): Unit =
			x = x + 1

	class Derived(x : Int, y: Int, z2: Int) extends Base(x, y + 1, z2): // warn // warn / for x, y translated to private[this] x field & shadowing var Base.x, Base.y
		private def hello() = 4
		private val shadowed2 = 2 + 2 // warn (In Scala 2 we cannot do that got the warning)
		private[this] val shadowed3 = 3 + 3 // warn
		//private[Derived] val fatalOverride = 0 //  value fatalOverride of type Int has weaker access privileges; it should be public
		private val shadowed5 = 5 + 5 // warn
		private val notShadowed2 = -4
		//protected var shadowed6 = 6 + 6 // variable shadowed6 of type Int has weaker access privileges; it should be public

		def inFunctionScope() =
			val notShadowed = -2 // OK
			-2

		override def toString =
			s"x : ${x.toString}, y : ${y.toString}"

	class UnderDerived(x: Int, y: Int, z: Int) extends Derived(x, y, z) // warn // warn // warn

	def main(args: Array[String]) =
		val derived = new Derived(1, 1, 1)
		println(derived.toString)     // yields x: '1', as expected
		derived.increment()
		println(derived.toString)     // still x: '1', probably unexpected, for y it never prints the super value, less surprising
		println(derived.shadowed2)
		println(derived.shadowed3)