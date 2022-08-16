enum SUB[-L, +R]:
  case Refl[C]() extends SUB[C, C]

trait Tag { type T }

def foo[B](g: Tag { type T >: B <: Int })(
    e: SUB[g.type, Tag { type T = Int }], i: Int,
): B = e match
  case SUB.Refl() =>
    // B = Nothing
    // g = Tag { T = Int..Int } <: Tag { T = B..Int }
    // SUB[g,                  Tag { T = Int..Int }]
    // SUB[Tag { T = B..Int }, Tag { T = Int..Int }] approxLHS
    // Int <: B
    i // error: Found: (i: Int) Required: B

def bad(i: Int): String =
  val g = new Tag { type T = Int }
  val e: SUB[g.type, Tag { type T = Int }] = SUB.Refl[g.type]()
  foo[Nothing](g)(e, i) // cast Int to String!

object Test:
  def main(args: Array[String]): Unit = bad(1) // was: ClassCastException: Integer cannot be cast to String
