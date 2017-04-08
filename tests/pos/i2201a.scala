class Foo[T]

class Fix[F[_]](unfix: F[Fix[F]])
object DocTree {
  type Const[T] = Foo[Int]
  type FixConst = Fix[Const]
  def docTree(s: Const[FixConst]): FixConst = new Fix(s)
}
