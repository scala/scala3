trait Bar
trait Baz

def f =
  identity(
    locally:
      trait Foo extends Bar, Baz
  )