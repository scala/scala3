import scala.quoted.*

trait Api:
  type Reader[E]

def bugImpl[T: Type, Q[_]: Type](using Quotes) =
  '{
    val p: Api = ???
    ${
      Type.of[p.Reader[T]]
      Type.of[Q[p.Reader[T]]]
      Type.of[p.Reader[Q[p.Reader[T]]]]
      Type.of[List[p.Reader[T]]]
      Type.of[p.Reader[List[p.Reader[T]]]]
      Type.of[p.Reader[List[T]]]
      Type.of[p.Reader[Q[T]]]
      Expr(1)
    }
  }
