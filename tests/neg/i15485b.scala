enum SUB[-L, +R]:
  case Refl[C]() extends SUB[C, C]

trait Tag { type T }

def foo[B, X <: Tag { type T >: B <: Int }](
    e: SUB[X, Tag { type T = Int }], i: Int,
): B = e match
  case SUB.Refl() =>
    // B = Nothing
    // X = Tag { T = Int..Int } <: Tag { T = B..Int }
    // SUB[X,                  Tag { T = Int..Int }]
    // SUB[Tag { T = B..Int }, Tag { T = Int..Int }] approxLHS
    // Int <: B
    i // error: Found: (i: Int) Required: B

def bad(i: Int): String =
  foo[Nothing, Tag { type T = Int }](SUB.Refl(), i)  // cast Int to String!

object Test:
  def main(args: Array[String]): Unit = bad(1) // was: ClassCastException: Integer cannot be cast to String
