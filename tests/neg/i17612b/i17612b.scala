//> using options -Xfatal-warnings -Xlint:private-shadow -source:3.3

object i17612b:

	trait Base(var x: Int, val y: Int, var z: Int):
		var shadowed2 = 2
		val shadowed3 = 3
		val shadowed4 = 4
		protected var shadowed5 = 5

		val notShadowed = -1
		private val notShadowed2 = -2
		val notShadowedbyLambda = -2

		def increment(): Unit =
			x = x + 1

	trait BaseB
	trait BaseC(var x2: Int)

	class Derived(x : Int, x3: Int, y: Int, z2: Int) extends BaseB, BaseC(x3), Base(x, y + 1, z2): // warn // warn / for x, y translated to private[this] x field & shadowing var Base.x, Base.y
		private def hello() = 4
		private val shadowed2 = 2 + 2 // warn (In Scala 2 we cannot do that got the warning)
		private[this] val shadowed3 = 3 + 3 // warn

		private val shadowed5 = 5 + 5 // warn
		private val notShadowed2 = -4

		val lambda: Int => Int => Int =
			notShadowedbyLambda =>
				notShadowedbyLambda =>
					notShadowedbyLambda * 2

		def inFunctionScope() =
			val notShadowed = -2 // OK
			-2

		override def toString =
			s"x : ${x.toString}, y : ${y.toString}"

	class UnderDerived(x: Int, y: Int, z: Int) extends Derived(x, 1, y, z) // warn // warn // warn

	def main(args: Array[String]) =
		val derived = new Derived(1, 1, 1, 1)
// nopos-error: No warnings can be incurred under -Werror.