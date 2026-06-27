// Fixes #16018: existential widening for wildcard arguments.
// Real-world reproductions: mbovel's minimization, a pure-Scala collection
// (no capture conversion), and the original akka report (a Java-generic
// argument re-projected as a Scala collection and `collect`-ed).
import scala.collection.immutable

object Test:

  class Box[T](val value: T)
  class Container[+S, +M]
  class SubContainer[+S, +M] extends Container[S, M]

  def f1[T](l: List[Box[? <: T]]): List[T] = l.map(_.value)

  def f2[M](xs: immutable.Seq[Container[Any, ? <: M]]): immutable.Seq[Container[Any, M]] =
    xs.collect {
      case g: SubContainer[Any, M] @unchecked => g
      case other                              => other
    }

  def seqOf[T](it: java.lang.Iterable[T]): immutable.Seq[T] = ???
  def f3[M](xs: java.util.List[? <: Container[Any, ? <: M]]): immutable.Seq[Container[Any, M]] =
    seqOf(xs).collect {
      case g: SubContainer[Any, M] @unchecked => g
      case other                              => other
    }

// The exact akka-minimized repro from the ticket (modulo `_` -> `?`).
object Main:

  class Source[+Out, +Mat] extends Graph[SourceShape[Out], Mat]
  class Shape
  class SourceShape[+T] extends Shape
  class Graph[+S <: Shape, +M]

  def immutableSeq[T](iterable: java.lang.Iterable[T]): immutable.Seq[T] = ???

  def combine[T, U, M](sources: java.util.List[? <: Graph[SourceShape[T], ? <: M]])
      : Source[U, java.util.List[M]] =
    val seq: immutable.Seq[Graph[SourceShape[T], M]] =
      if sources != null then
        immutableSeq(sources).collect {
          case source: Source[T, M] @unchecked => source
          case other                           => other
        }
      else immutable.Seq()
    ???
