import scala.util.boundary
import scala.compiletime.uninitialized
import runtime.suspend

trait CanProduce[-T]:
  def produce(x: T): Unit

object generate:

  def produce[T](x: T)(using cp: CanProduce[T]): Unit = cp.produce(x)

  def apply[T](body: CanProduce[T] ?=> Unit): Iterator[T] = new:
    var nextKnown: Boolean = false
    var nextElem: Option[T] = uninitialized

    var step: () => Unit = () =>
      boundary[Unit]:
        given CanProduce[T]:
          def produce(x: T): Unit =
            nextElem = Some(x)
            suspend[Unit, Unit]: k =>
              step = () => k.resume(())
        body
      nextElem = None

    def hasNext: Boolean =
      if !nextKnown then { step(); nextKnown = true }
      nextElem.isDefined

    def next: T =
      require(hasNext)
      nextKnown = false
      nextElem.get
end generate

enum Tree[T]:
  case Leaf(x: T)
  case Inner(xs: List[Tree[T]])

def leafs[T](t: Tree[T]): Iterator[T] =
  generate:
    def recur(t: Tree[T]): Unit = t match
      case Tree.Leaf(x) => generate.produce(x)
      case Tree.Inner(xs) => xs.foreach(recur)
    recur(t)

object Variant2:
  trait Generator[T]:
    def nextOption: Option[T]

  def generate[T](body: CanProduce[T] ?=> Unit): Generator[T] = new:

    def nextOption: Option[T] =
      step()

    var step: () => Option[T] = () =>
      boundary:
        given CanProduce[T]:
          def produce(x: T): Unit =
            suspend[Unit, Option[T]]: k =>
              step = () => k.resume(())
              Some(x)
        body
        None
  end generate


