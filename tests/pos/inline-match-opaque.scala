object Foo:
  opaque type Wrapper[T] = T
  def part[T](w: Wrapper[T]): T = w
  inline def part2[T](w: Wrapper[T]): T = part(w) //part(w.asInstanceOf[Wrapper[T]]) also fixes the issue
  type Rewrap[W] = Wrapper[Extra.Unwrap[W]]

object Extra:
  type Unwrap[W] = W match
    case Foo.Wrapper[t] => t
  type Rewrap[W] = Foo.Wrapper[Unwrap[W]]

object Test:
  type X = Extra.Rewrap[Foo.Wrapper[Int]]
  def foo1(x: Foo.Wrapper[Extra.Unwrap[Foo.Wrapper[Int]]]) =
    Foo.part(x) // ok
    Foo.part2(x) // ok
  def foo2(x: X) =
    Foo.part(x) // ok
    Foo.part2(x) // error
