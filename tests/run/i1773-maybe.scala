//> using options -Yexplicit-nulls
import language.experimental.magic
object Test {
  into class Foo(sc: StringContext) {
    object q {
      def unapply(arg: Any): (Any, Any)? =
        (sc.parts(0), sc.parts(1))
    }
  }
  given Conversion[StringContext, Foo] = Foo(_)

  def main(args: Array[String]): Unit = {
    val q"class ${name: String} extends ${parent: String}" = (new Object).runtimeChecked
    println(name)
    println(parent)
  }
}
