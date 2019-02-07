object `gadt-accumulatable` {
  sealed abstract class Or[+G,+B] extends Product with Serializable
  final case class Good[+G](g: G) extends Or[G,Nothing]
  final case class Bad[+B](b: B) extends Or[Nothing,B]

  sealed trait Validation[+E] extends Product with Serializable
  case object Pass extends Validation[Nothing]
  case class Fail[E](error: E) extends Validation[E]

  sealed abstract class Every[+T] protected (underlying: Vector[T]) extends /*PartialFunction[Int, T] with*/ Product with Serializable
  final case class One[+T](loneElement: T) extends Every[T](Vector(loneElement))
  final case class Many[+T](firstElement: T, secondElement: T, otherElements: T*) extends Every[T](firstElement +: secondElement +: Vector(otherElements: _*))

  class Accumulatable[G, ERR, EVERY[_]] { }

  def convertOrToAccumulatable[G, ERR, EVERY[b] <: Every[b]](accumulatable: G Or EVERY[ERR]): Accumulatable[G, ERR, EVERY] = {
    new Accumulatable[G, ERR, EVERY] {
      def when[OTHERERR >: ERR](validations: (G => Validation[OTHERERR])*): G Or Every[OTHERERR] = {
        accumulatable match {
          case Good(g) =>
            val results = validations flatMap (_(g) match { case Fail(x) => val z: OTHERERR = x; Seq(x); case Pass => Seq.empty})
            results.length match {
              case 0 => Good(g)
              case 1 => Bad(One(results.head))
              case _ =>
                val first = results.head
                val tail = results.tail
                val second = tail.head
                val rest = tail.tail
                Bad(Many(first, second, rest: _*))
            }
          case Bad(myBad) => Bad(myBad)
        }
      }
    }
  }
}
