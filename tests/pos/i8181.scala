object Main:
  def main(args: Array[String]): Unit =
    extension (a: AnyRef)
      def putout(): Unit = println(a)

    "blub".putout()
