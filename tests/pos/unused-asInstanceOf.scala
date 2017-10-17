
trait Dataset {
  def select(unused c: Column): Unit = ()
}

class Column

object Test {
  def main(args: Array[String]): Unit = {

    val ds: Dataset = ???

    unused val collD = new Column

    ds.select(collD)

  }
}
