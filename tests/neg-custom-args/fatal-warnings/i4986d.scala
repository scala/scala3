trait Foo[A]

type Fooable[A] = {
  def foo(implicit @annotation.implicitNotFound("There's no Foo[${A}]") ev: Foo[A]): Any // error

  type InnerFooable = {
    def foo(implicit @annotation.implicitNotFound("There's no Foo[${A}]") ev: Foo[A]): Any // error
  }
}

object Fooable {
  (null: Fooable[Long]).foo // error
  (null: Fooable[Long]#InnerFooable).foo // error
}


@annotation.implicitNotFound("There's no Bar[${A}]")
trait Bar[A]

type Barable[A] = {
  def bar(implicit ev: Bar[A]): Any // ok
}
