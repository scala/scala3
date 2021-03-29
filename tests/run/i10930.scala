import language.future
import compiletime.asMatchable

@main def Test =
  type LeafElem[X] = X match
    case String => Char
    case Array[t] => LeafElem[t]
    case Iterable[t] => LeafElem[t]
    case AnyVal => X

  def leafElem[X](x: X): LeafElem[X] = x.asMatchable match
    case x: String      => x.charAt(0)
    case x: Array[t]    => leafElem(x(1))
    case x: Iterable[t] => leafElem(x.head)
    case x: AnyVal      => x

  def f[X](x: X) = x

  def leafElem2[X](x: X): LeafElem[X] = f(x).asMatchable match
    case x: String      => x.charAt(0)
    case x: Array[t]    => leafElem(x(1))
    case x: Iterable[t] => leafElem(x.head)
    case x: AnyVal      => x

  val x1: Char = leafElem("a")
  assert(x1 == 'a')
  val x2: Char = leafElem(Array("a", "b"))
  assert(x2 == 'b')
  val x3: Char = leafElem(List(Array("a", "b"), Array("")))
  assert(x3 == 'b')
  val x4: Int = leafElem(3)
  assert(x4 == 3)

