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

class Cell(elem: String) {
  override def toString = s"Cell($elem)"
}

object Test {

  def table(init: implicit Table => Unit) = {
    implicit val t = new Table
    init
    t
  }

  def row(init: implicit Row => Unit)(implicit t: Table) = {
    implicit val r = new Row
    init
    t.add(r)
  }

  def cell(str: String)(implicit r: Row) =
    r.add(new Cell(str))

  val data =
    table {
      row {
        cell("A1")
        cell("B1")
      }
      row {
        cell("A2")
        cell("B2")
      }
    }

  def main(args: Array[String]) = {
    println(data)
  }
}
