import quoted._

object Test {
  def main(args: Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)

    val tb = Toolbox.make
    println(tb.show {
      '{
        val a = '{4}
        ${'{
          '{${a}}
        }}
      }
    })
  }
}
