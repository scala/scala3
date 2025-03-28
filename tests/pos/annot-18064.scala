//> using options "-Vprint:typer"

class myAnnot[T]() extends annotation.Annotation

trait Tensor[T]:
  def add: Tensor[T] @myAnnot[T]()

class TensorImpl[A]() extends Tensor[A]:
  def add /* : Tensor[A] @myAnnot[A] */ = this
