import reflect.Selectable.reflectiveSelectable

trait Foo:
  def f(): Long

def h() = k((_: Foo) => ???)

trait Bar[TB]
given Bar[Foo] = ???

def k[Tk, Ptr <: { def f(): Tk }](function: Ptr => Int)(using alloc: Bar[Ptr]): Tk = ???
