
def kek(t: Table, ids: t.Id*) = ???

trait Table {
  type Id = String
}

object Table1 extends Table {
  val id: Id = "table1_id"
}

class Table2() extends Table {
  val id: Id = "table2_id"
}

val x = kek(Table1, Table1.id)
val y = kek(Table2(), Table2().id)