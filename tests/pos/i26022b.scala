// Multiple chained opaque aliases in the same module must not produce a
// self-referential proxy: the rewrite of refinement RHS to use the
// just-created proxy would otherwise make $proxy.<member> point back into
// the same proxy, which the avoid-local-references walk explodes on.
// Reproduces the lepus regression.

object Domains:
  opaque type ShortString <: String = String
  opaque type ExchangeName <: ShortString = ShortString
  opaque type ExchangeType <: ShortString = ShortString
  opaque type QueueName <: ShortString = ShortString
  opaque type Path <: ShortString = ShortString

  object ShortString:
    inline def apply(s: String): ShortString = s

  object ExchangeName:
    inline def apply(s: String): ExchangeName = ShortString(s)

  object QueueName:
    inline def apply(s: String): QueueName = ShortString(s)

import Domains.*

class Use:
  val q: QueueName = QueueName("q")
  val e: ExchangeName = ExchangeName("e")
