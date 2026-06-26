case class Box[A](value: A)

class Category1Hi[F[_ <: AnyRef]]
class Category1LoHi[F[_ >: String <: AnyRef]]

final class VCInt(val x: Int) extends AnyVal
final class VCString(val s: String) extends AnyVal

class HK[F[_]]

class C {
  def funcA: Category1Hi[[X <: AnyRef] =>> Box[X]] = null
  def funcB: Category1LoHi[[X >: String <: AnyRef] =>> Box[X]] = null
  // type lambdas whose result is not an applied type: value classes are boxed, primitives use their boxed class
  def funcC: HK[[X] =>> VCInt] = null
  def funcD: HK[[X] =>> VCString] = null
  def funcE: HK[[X] =>> Int] = null
}

object Test:
  def main(args: Array[String]): Unit =
    classOf[C].getMethods.filter(_.getName.startsWith("func")).sortBy(_.getName).foreach(m =>
      println(m)
      println(m.toGenericString)
    )
