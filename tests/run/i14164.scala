
object Test:
  class Base(a: String = "x", param: String)

  class Child extends Base(
    param =
      for x <- Seq("a") yield x
      "param"
  )

  new Child

  def main(args: Array[String]) = ()

end Test

class Test2:
  class Inner(withDefault: String = "inner")(
      dependentDefault: String = withDefault) extends Object {
    def this(x: Int) = this(x.toString)()
  }

class Test3:
  class Inner(withDefault: () => String = () => "inner")(
      dependentDefault: String = withDefault()) extends Object {
    def this(x: Int) = this(() => x.toString)()
  }
