import scala.language.dynamics

object Test {
  implicit class DynamicValue[T](val value: T) extends AnyVal with Dynamic {
    def applyDynamic(name: String)(args: Any*) = println(s"""$this.$name(${args mkString ", "})""")
    override def toString = "" + value
  }
  implicit class DynamicValue2[T](val value: T) extends Dynamic {
    def applyDynamic(name: String)(args: Any*) = println(s"""$this.$name(${args mkString ", "})""")
    override def toString = "" + value
  }

  def f[T](x: DynamicValue[T]) = x.dingo("bippy", 5)
  def g[T](x: DynamicValue2[T]) = x.dingo("bippy", 5)

  def main(args: Array[String]): Unit = {
    f(10)
    f(List(1, 2, 3))
    g(10)
    g(List(1, 2, 3))
  }
}
