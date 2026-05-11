case class Box[A](value: A)

class Category1Hi[F[_ <: AnyRef]]
class Category1LoHi[F[_ >: String <: AnyRef]]

class C {
  def funcA: Category1Hi[[X <: AnyRef] =>> Box[X]] = null
  def funcB: Category1LoHi[[X >: String <: AnyRef] =>> Box[X]] = null
}

object Test:
  def main(args: Array[String]): Unit =
    classOf[C].getMethods.filter(_.getName.startsWith("func")).sortBy(_.getName).foreach(m =>
      println(m)
      println(m.toGenericString)
    )
