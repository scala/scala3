import scala.quoted._

inline def transform(using dummyImplicit: DummyImplicit): Unit =
  ${ transformImpl } // error

def transformImpl(using s: Scope)(using dummyImplicit: DummyImplicit): s.Expr[Unit] = ???
