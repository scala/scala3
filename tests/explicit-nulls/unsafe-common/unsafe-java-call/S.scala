import scala.language.unsafeNulls

val j: J = new J

val s1: String = j.f1()

val i1: Int = j.f2()

val s2: String = j.g1[String]()

val i2: Int = j.g1[Int]()

val a1: Any = j.g1[Any]()

val ar1: AnyRef = j.g1[AnyRef]()

val n1: Null = j.g1[Null]()

val ar2: AnyRef = j.g1[Null]()

def clo[T]: T = j.g1[T]()