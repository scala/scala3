trait AbstractTable[T]

trait Query[E, U]

class TableQuery[E <: AbstractTable[?]] extends Query[E, Extract[E]]

type Extract[E] = E match
  case AbstractTable[t] => t

trait BaseCrudRepository[E[T[_]]]:

  type EntityTable <: AbstractTable[E[Option]]

  def filterById: Query[EntityTable, Extract[EntityTable]] =
    new TableQuery[EntityTable]
