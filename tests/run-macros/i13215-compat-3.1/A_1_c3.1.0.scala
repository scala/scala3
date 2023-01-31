package foo

trait Bar:
  inline def baz = Baz

private[foo] object Baz
