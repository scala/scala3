object Test:
  class Base(a: String = "x", param: String)

  class Child extends Base(
    param =
      for x <- Seq("a") yield x
      "param"
  )

  new Child

  def main(args: Array[String]) = ()

