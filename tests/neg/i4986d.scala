//> using options -Werror

trait Foo[A]

type Fooable[A] = {
  def foo(implicit @annotation.implicitNotFound("There's no Foo[${A}]") ev: Foo[A]): Any // warn

  type InnerFooable = {
    def foo(implicit @annotation.implicitNotFound("There's no Foo[${A}]") ev: Foo[A]): Any // warn
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
// nopos-error: No warnings can be incurred under -Werror (or -Xfatal-warnings)
