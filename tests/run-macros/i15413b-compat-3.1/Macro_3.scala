package foo
class C:
  inline def baz = D.bazImpl

object D:
  private[foo] def bazImpl = {}
