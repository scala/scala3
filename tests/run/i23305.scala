//> using options -language:experimental.erasedDefinitions

trait DBMeta[A] extends compiletime.Erased

trait Table[A]

case class Saved[A]()
object Saved {
  def table[A](using dbm: DBMeta[A]): Table[Saved[A]] = new Table[Saved[A]] {}
}

case class deleteBulk[A](as: List[Saved[A]])(using dbm: DBMeta[A], tbl: Table[A]) {
  final given savedTable: Table[Saved[A]] = Saved.table[A]
}

case class Foo()
object Foo {
  given dbMeta: DBMeta[Foo] = new DBMeta[Foo] {}
  given table: Table[Foo] = new Table[Foo] {}
}

@main def Test =
  val del = deleteBulk(List(Saved[Foo]()))
  del.savedTable
