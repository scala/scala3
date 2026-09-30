trait BasicBackend { type Database >: Null <: AnyRef }
trait JdbcBackend extends BasicBackend { class JdbcDatabaseDef; type Database = JdbcDatabaseDef }
trait BasicProfile { type Backend <: BasicBackend }
trait JdbcProfile extends BasicProfile { type Backend = JdbcBackend }

trait Test:
  def a: JdbcProfile#Backend#Database = null
  def b: JdbcBackend#Database = null
  def c(x: JdbcBackend#Database): JdbcBackend#JdbcDatabaseDef = x
  def d(x: JdbcBackend#JdbcDatabaseDef): JdbcBackend#Database = x
