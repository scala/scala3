import scala.reflect.Selectable.reflectiveSelectable

object Test {
  type Runner = { def run(args: Array[String]): Unit }

  def test(args: Array[String], runner: Runner): Unit =
    runner.run(args)

  def main(args: Array[String]): Unit = {
    test(Array("foo", "bar"), new {
      def run(args: Array[String]): Unit = args foreach println
    })
  }
}
