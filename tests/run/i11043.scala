// https://github.com/scala/scala3/issues/11043
import scala.reflect.Selectable.reflectiveSelectable

object Test {
  type Runner = { def run(args: Array[String]): Unit }

  def test(args: Array[String], runner: Runner): Unit =
    runner.run(args)

  def main(args: Array[String]): Unit =
    val out = new StringBuilder
    test(Array("foo", "bar"), new {
      def run(args: Array[String]): Unit = args.foreach(s => out.append(s).append('\n'))
    })
    assert(out.toString == "foo\nbar\n", out.toString)
}
