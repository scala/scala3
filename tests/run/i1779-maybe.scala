//> using options -Yexplicit-nulls
import language.experimental.magic
object Test {
  implicit class Foo(sc: StringContext) {
    object q {
      def unapply(arg: Any): (Any, Any)? =
        (sc.parts(0), sc.parts(1))
    }
  }

  def main(args: Array[String]): Unit = {
    val q"class $_ extends $_parent" = new Object
    println(_parent)
  }
}
