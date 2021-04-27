
import Macros.*

object Test {
  def main(args: Array[String]): Unit = {
    let(1)(x => { println(x); println(x) })
    let(null)(x => { println(x); println(x) })
    let {
      println("foo")
      "bar"
    } { x =>
      println(x)
      println(x)
    }
  }

}
