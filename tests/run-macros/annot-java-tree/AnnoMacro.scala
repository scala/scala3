import scala.quoted.*

inline def checkSuppressWarnings[T]: Unit = ${ checkSuppressWarningsImpl[T] }

def checkSuppressWarningsImpl[T: Type](using Quotes): Expr[Unit] =
  import quotes.reflect.*
  val SuppressWarningsSymbol = TypeTree.of[SuppressWarnings].symbol
  val sym = TypeRepr.of[T].typeSymbol
  // Imitate what wartremover does, so we can avoid unintentionally breaking it:
  // https://github.com/wartremover/wartremover/blob/fb18e6eafe9a47823e04960aaf4ec7a9293719ef/core/src/main/scala-3/org/wartremover/WartUniverse.scala#L63-L77
  val actualArgs = sym
    .getAnnotation(SuppressWarningsSymbol)
    .collect {
      case Apply(
            Select(_, "<init>"),
            Apply(Apply(_, Typed(Repeated(values, _), _) :: Nil), Apply(_, _ :: Nil) :: Nil) :: Nil
          ) =>
        // "-Yexplicit-nulls"
        // https://github.com/wartremover/wartremover/issues/660
        values.collect { case Literal(StringConstant(str)) =>
          str
        }
    }
    .toList
    .flatten
  val expectedArgs = List("a", "b")
  assert(actualArgs == expectedArgs,
    s"Expected $expectedArgs arguments for SuppressWarnings annotation of $sym but got $actualArgs")
  '{}
