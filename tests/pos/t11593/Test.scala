package foo {
  import java.util.*
  object X {
    def bar(x: Properties): Unit = println(x.getClass.getName)
    bar(new Properties)
  }
}
object Test extends App {
  foo.X
}