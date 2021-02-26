import scala.language.implicitConversions

trait TripleEqualsSupport:
  class Equalizer[L](val leftSide: L)
  def convertToEqualizer[T](left: T): Equalizer[T]

trait TripleEquals extends TripleEqualsSupport:
  implicit override def convertToEqualizer[T](left: T): Equalizer[T] = new Equalizer(left)

class GraphDB[Id]:
  class Node private[GraphDB](val id: Id)

object GraphDBSpec extends TripleEquals:
  object graph extends GraphDB[String]
  import graph.Node
  val m = new Node("Alice") // error
