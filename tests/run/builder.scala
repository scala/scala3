import collection.mutable.ArrayBuffer

class Table {
  val rows = new ArrayBuffer[Row]
  def add(r: Row): Unit = rows += r
  override def toString = rows.mkString("Table(", ", ", ")")
}

class Row {
  val cells = new ArrayBuffer[Cell]
  def add(c: Cell): Unit = cells += c
  override def toString = cells.mkString("Row(", ", ", ")")
}

case class Cell(elem: String)

object Test {

  def table(init: given Table => Unit) = {
    implicit val t = new Table
    init
    t
  }

  def row(init: given Row => Unit)(implicit t: Table) = {
    implicit val r = new Row
    init
    t.add(r)
  }

  def cell(str: String)(implicit r: Row) =
    r.add(new Cell(str))

  val data =
    table {
      row {
        cell("top left")
        cell("top right")
      }
      row {
        cell("botttom left")
        cell("bottom right")
      }
    }

  def main(args: Array[String]) = {
    println(data)
  }
}
