// https://github.com/scala/scala3/issues/26018
import scala.compiletime.summonInline

trait Hammer[I, O]
trait HasT { type T }

object Lib:
  inline given derived[I, O](using h: HasT): Hammer[I, O] =
    val _ = summonInline[Hammer[I, h.T]]
    new Hammer[I, O] {}

import Lib.given

class B
class D
given HasT with { type T = B }

@main def m(): Unit =
  val h: Hammer[D, D] = summon[Hammer[D, D]] // error
