object Main:
  def main(args: Array[String]): Unit =
     inline def v2 = InlineMac.sample("foo")
     inline def v1 = v2

     v2  // error
