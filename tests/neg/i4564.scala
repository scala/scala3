object ClashOverloadNoSig {

  private def apply(x: Int) = if (x > 0) new ClashOverloadNoSig(x) else apply("") // error: overloaded method apply needs result type

  def apply(x: String): ClashOverloadNoSig = ???
}

case class ClashOverloadNoSig private(x: Int)

object ClashRecNoSig {
  private def apply(x: Int) = if (x > 0) ClashRecNoSig(1) else ???    // error: recursive method apply needs result type
}

case class ClashRecNoSig private(x: Int)

object NoClashNoSig {
  private def apply(x: Boolean) = if (x) NoClashNoSig(1) else ???   // error: overloaded method apply needs result type
}

case class NoClashNoSig private(x: Int)

object NoClashOverload {
  private def apply(x: Boolean) = if (x) NoClashOverload(1) else apply("")   // error // error: overloaded method apply needs result type (twice)

  def apply(x: String): NoClashOverload = ???
}

case class NoClashOverload private(x: Int)


class BaseNCNSP[T] {
  def apply(x: T) = if (???) NoClashNoSigPoly(1) else ???    // error: overloaded method apply needs result type
}

object NoClashNoSigPoly extends BaseNCNSP[Boolean]
case class NoClashNoSigPoly private(x: Int)   // ok, since `apply` is in base class



class BaseCNSP[T] {
  def apply(x: T) = if (???) ClashNoSigPoly(1) else ???    // error: recursive method apply needs result type
}

object ClashNoSigPoly extends BaseCNSP[Int]
case class ClashNoSigPoly private(x: Int)
