enum SUB[-L, +R]:
  case Refl[C]() extends SUB[C, C]

trait Tag { type T }

def foo[B, X <: Tag { type T <: Int }](
    e: SUB[X, Tag { type T <: B }], i: Int,
): B = e match
  case SUB.Refl() =>
    // B = String
    // X = Tag { T = Nothing..Nothing } <: Tag { T = Nothing..Int }
    // SUB[X,                        Tag { T = Nothing..B }]
    // SUB[Tag { T = Nothing..Int }, Tag { T = Nothing..B }] approxLHS
    //                        Int   <:    B
    i // error: Found: (i: Int) Required: B

def bad(i: Int): String =
  foo[String, Tag { type T = Nothing }](SUB.Refl(), i)  // cast Int to String

object Test:
  def main(args: Array[String]): Unit = bad(1) // was: ClassCastException: Integer cannot be cast to String
