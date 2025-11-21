import scala.annotation.valhalla

@valhalla
class A extends AnyVal with DeepValhalla

@valhalla
trait B extends Any with DeepValhalla

@valhalla
case class C(c1: Int, c2: A) extends AnyVal with B

@valhalla class D(val a: A, val b: Int, val c: C, val d: B) extends AnyVal with DeepValhalla
