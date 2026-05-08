package fix

object OptionOrNullMigrationTest {

  // scalikejdbc/ResultSetExtractorException.scala:9 shape
  val e: Option[Exception] = None
  val msg = e.getOrElse(null.asInstanceOf[Exception])

  // scalikejdbc/SQL.scala:123 shape
  val pairs: Seq[(String, Any)] = Nil
  val v: Any =
    pairs
      .find(_._1 == "x")
      .map(_._2)
      .getOrElse(null.asInstanceOf[Any])

  // scalikejdbc-async/RowDataResultSet.scala:625 shape
  val rows: Seq[Map[String, Any]] = Nil
  val cell: Any = rows.headOption.map(_.get("c")).getOrElse(null.asInstanceOf[Any])

  // norbert-radyk/spoiwo/Utils.scala:44 shape — generic with explicit Null<:<T
  def cmp[T](actualData: Option[T], expectedData: T)(using ev: Null <:< T): Boolean =
    actualData.getOrElse(null.asInstanceOf[T]) == expectedData

  // ruimo/scoins/PathUtil.scala:41 shape (note: type ascription not the bare form)
  val prefix: Option[String] = None
  val name: String = prefix.getOrElse(null.asInstanceOf[String])

  // Bare .orNull — should NOT be rewritten, just lint
  val bare: Option[String] = None
  val asNullable = bare.orNull 
}
