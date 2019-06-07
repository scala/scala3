
trait Dataset {
  def select erased (c: Column): Unit = ()
}

class Column

object Test {
  def main(args: Array[String]): Unit = {

    val ds: Dataset = ???

    lazy val collD = new Column

    ds.select(collD)

  }
}
