/* Dies with
   java.util.NoSuchElementException: key not found: val <none>
	at scala.collection.MapLike$class.default(MapLike.scala:228)
	at scala.collection.AbstractMap.default(Map.scala:59)
	at scala.collection.MapLike$class.apply(MapLike.scala:141)
	at scala.collection.AbstractMap.apply(Map.scala:59)
	at dotty.tools.dotc.backend.jvm.DottyPrimitives.getPrimitive(scalaPrimitives.scala:46)
	at dotty.tools.backend.jvm.DottyBackendInterface$$anon$2.getPrimitive(DottyBackendInterface.scala:157)
	at dotty.tools.backend.jvm.DottyBackendInterface$$anon$2.getPrimitive(DottyBackendInterface.scala:153)
	at scala.tools.nsc.backend.jvm.BCodeBodyBuilder$PlainBodyBuilder.liftStringConcat(BCodeBodyBuilder.scala:1147)
*/
class Foo
class Test {
	def update[B](x : B, b : Int): Unit = {}
	def apply[B](x : B) = 1
}
class Test2 {
  type B = Foo
	def update(x : B, b : Int): Unit = {}
	def apply(x : B) = 1
}

object Test {
	def main(a : Array[String]): Unit = {
		val a = new Test
		val f = new Foo
		a(f) = 1 //works
		a(f) = a(f) + 1 //works
		a(f) += 1 //error: reassignment to val
	}
}
object Test2 {
	def main(args : Array[String]): Unit = {
                args(0) += "a"
		val a = new Test2
		val f = new Foo
		a(f) = 1 //works
		a(f) = a(f) + 1 //works
		a(f) += 1 //error: reassignment to val
	}
}
