import scala.annotation.Annotation
class myRefined(f: ? => Boolean) extends Annotation

def test(axes: Int) = true

trait Tensor:
  def mean(axes: Int): Int @myRefined(_ => test(axes))

class TensorImpl() extends Tensor:
  def mean(axes: Int) = ???
