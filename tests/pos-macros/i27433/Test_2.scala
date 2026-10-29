package repro

import scala.compiletime.{erasedValue, error, summonInline}
import scala.deriving.Mirror

enum Touched { case A, B }
enum Untouched { case A, B }

object Touch {
  Macros.summonCaseMirrors[Touched]
}

object Check {
  private inline def requireSingleton[T]: Unit =
    inline summonInline[Mirror.Of[T]] match {
      case _: Mirror.Singleton => ()
      case _ => error("not a Mirror.Singleton")
    }

  private inline def all[Ts <: Tuple]: Unit =
    inline erasedValue[Ts] match {
      case _: EmptyTuple => ()
      case _: (h *: t) => requireSingleton[h]; all[t]
    }

  inline def allCasesAreSingletons[E](using m: Mirror.SumOf[E]): Unit = all[m.MirroredElemTypes]
}

object Use {
  Check.allCasesAreSingletons[Untouched]
  Check.allCasesAreSingletons[Touched]
}
