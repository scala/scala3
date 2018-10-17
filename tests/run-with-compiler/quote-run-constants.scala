import scala.quoted._
import scala.quoted.autolift._

object Test {
  def main(args: Array[String]): Unit = {
    val tb = Toolbox.make
    import tb._

    println(run(true))
    println(run('a'))
    println(run('\n'))
    println(run('"'))
    println(run('\''))
    println(run('\\'))
    println(run(1))
    println(run(2))
    println(run(3L))
    println(run(4.0f))
    println(run(5.0d))
    println(run("xyz"))

    println("======")

    println(show(true))
    println(show('a'))
    println(show('\n'))
    println(show('"'))
    println(show('\''))
    println(show('\\'))
    println(show(1))
    println(show(2))
    println(show(3L))
    println(show(4.0f))
    println(show(5.0d))
    println(show("xyz"))
    println(show("\n\\\"'"))
    println(show("""abc
         xyz"""))
  }
}
