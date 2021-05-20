sealed case class Column[A](name: String)

sealed trait ColumnSet {
  type Append[That <: ColumnSet] <: ColumnSet
  def ++[That <: ColumnSet](that: That): Append[That]
}

object ColumnSet {
  type Empty                  = Empty.type
  type Singleton[A]           = Cons[A, Empty]

  case object Empty extends ColumnSet {
    type Append[That <: ColumnSet] = That
    override def ++[That <: ColumnSet](that: That): Append[That] = that
  }

  sealed case class Cons[A, B <: ColumnSet](head: Column[A], tail: B) extends ColumnSet { self =>
    type Append[That <: ColumnSet] = Cons[A, tail.Append[That]]
    override def ++[That <: ColumnSet](that: That): Append[That] = Cons(head, tail ++ that)
  }

  def long(name: String): Singleton[Long]     = Cons(Column[Long](name), Empty)
  def string(name: String): Singleton[String] = Cons(Column[String](name), Empty)
}

object Example {
  import ColumnSet._
  val schema0 = long("id") ++ string("first_name")

  // inferred type 3.0.0-RC3: Singleton[Long]#Append[Cons[String, Empty]]#Append[Singleton[String]]
  // inferred type 2.13.5   : Cons[Long,Cons[String,Singleton[String]]]
  val schema1 = long("id") ++ string("first_name") ++ string("last_name")

  // inferred type 3.0.0-RC3: error
  // inferred type 2.13.5   : Cons[Long,Cons[String,Cons[String,Singleton[Long]]]]
  val schema2 = long("id") ++ string("first_name") ++ string("last_name") ++ long("age")

  // inferred type 3.0.0-RC3: Singleton[Long]#Append[Cons[String, Empty]]#Append[Singleton[String]]#Append[Cons[Long, Empty]]
  val schema3 = ((long("id") ++ string("first_name") ++ string("last_name")): Singleton[Long]#Append[ColumnSet.Cons[String, ColumnSet.Empty]]#Append[ColumnSet.Singleton[String]]) ++ long("age")
}
