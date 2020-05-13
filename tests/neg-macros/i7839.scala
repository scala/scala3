import scala.quoted._

inline def transform(using dummyImplicit: DummyImplicit): Unit =
  ${ transformImpl } // error

def transformImpl(using dummyImplicit: DummyImplicit)(using QuoteContext): Expr[Unit] = ???
