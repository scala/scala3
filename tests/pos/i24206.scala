class DispatchQuery:
  type CompareFunction = (AnyRef, AnyRef) => Boolean
  private var _sortedWith: Option[CompareFunction] = Option.empty

  def sortedWith(f: PartialFunction[(AnyRef, AnyRef), Boolean]): DispatchQuery = {
    // fails
    _sortedWith = Option { case (a, b) => f.applyOrElse((a, b), _ => false) }
    // workaround
    _sortedWith = Option[CompareFunction] { case (a, b) => f.applyOrElse((a, b), _ => false) }
    this
  }

trait Result
def getAll(nameFilter: Option[String => Boolean]): List[Result] = ???
def get(collectionName: String): List[Result] =
    getAll(Option(_.startsWith(collectionName)))

def f[T](x: T | Null): T = ???
val _: Any => Any = f(x => x)
