//> using options -Xfatal-warnings

class Test {
  def test[X <: Option[Int]](x: X) = x.isInstanceOf[Some[Int]]
  def test1[Y <: Int, X <: Option[Y]](x: X) = x.isInstanceOf[Some[Int]]
  def test2(x: Any) = x.isInstanceOf[Function1[Nothing, _]]
  def test3a(x: Any) = x.isInstanceOf[Function1[Any, _]] // warn
  def test3b(x: Any) = x.isInstanceOf[Function1[Int, _]] // warn
  def test4[Y <: Int, X <: Function1[Y, Unit]](x: X) = x.isInstanceOf[Function1[Int, _]] // warn
  def test5[Y <: Int, X <: Function1[Y, Unit]](x: X) = x.isInstanceOf[Function1[Int, Unit]]  // warn
  def test6[Y <: Int, X <: Function1[Y, Unit]](x: X) = x.isInstanceOf[Function1[Int, Any]] // warn
  def test7[Y <: Int, X <: Function1[Y, Unit]](x: X) = x.isInstanceOf[Function1[_, Unit]]
}
// nopos-error: No warnings can be incurred under -Werror.