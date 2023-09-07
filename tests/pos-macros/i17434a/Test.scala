// test.scala
import scala.language.dynamics

trait SQLSyntaxProvider[A] extends Dynamic{
  def field(name: String): SQLSyntax = ???

  inline def selectDynamic(inline name: String): SQLSyntax =
  select[A](this, name)

  inline def select[E](ref: SQLSyntaxProvider[A], inline name: String): SQLSyntax =
    ${ SelectDynamicMacroImpl.selectImpl[E]('ref, 'name) }
}

class SQLSyntax(value: String)
trait SQLSyntaxSupport[A]
case class ColumnSQLSyntaxProvider[S <: SQLSyntaxSupport[A], A](support: S) extends SQLSyntaxProvider[A]

case class Account(id: Long, name: String)
object Account extends SQLSyntaxSupport[Account]

def Test() =
  val p = ColumnSQLSyntaxProvider[Account.type, Account](Account)
  assert(p.name == SQLSyntax("name"))
