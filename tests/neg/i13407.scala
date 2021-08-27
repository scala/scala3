import scala.quoted.Type

trait Tensor[S <: Tuple] {
  def sum[Axis <: Shape: Type](axis: Axis): Tensor[S] = { // error
    Tensor.mk
  }
}

object Tensor {
  def mk[S <: Tuple]: Tensor[S] = new Tensor {}
}

object Foo {
  val t1: Tensor[("batch", "len", "embed")] = Tensor.mk
   def foo(x: Any) = {

  }
  foo(foo(t1.sum("len"))) // error
}
