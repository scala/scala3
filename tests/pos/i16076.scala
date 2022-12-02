trait Column[V]
trait ColumnPath

trait ColumnFactory[V, C <: Column[V]]:
  def apply(columnPath: ColumnPath): C

object ColumnFactory:
  private def apply[V, C <: Column[V]](f: String => C): ColumnFactory[V, C] =
    columnPath => f(columnPath.toString())
