

transparent sealed trait TA
transparent sealed trait TB
trait S
case object a extends S, TA, TB
case object b extends S, TA, TB

object Test:

  def choose0[X](x: X, y: X): X = x
  def choose1[X <: TA](x: X, y: X): X = x
  def choose2[X <: TB](x: X, y: X): X = x
  def choose3[X <: Product](x: X, y: X): X = x
  def choose4[X <: TA & TB](x: X, y: X): X = x

  choose0(a, b) match
    case _: TA => ???
    case _: TB => ???  // warn: unreachable

  choose1(a, b) match
    case _: TA => ???
    case _: TB => ???  // warn: unreachable

  choose2(a, b) match
    case _: TB => ???
    case _: TA => ???  // warn: unreachable

  choose3(a, b) match
    case _: Product => ???
    case _: TA => ???  // warn: unreachable

  choose4(a, b) match
    case _: (TA & TB) => ???
    case _: Product => ???  // warn: unreachable