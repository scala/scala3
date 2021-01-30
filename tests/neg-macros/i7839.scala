import scala.quoted.*

inline def transform(using dummyImplicit: DummyImplicit): Unit =
  ${ transformImpl } // error

def transformImpl(using dummyImplicit: DummyImplicit)(using Quotes): Expr[Unit] = ???
