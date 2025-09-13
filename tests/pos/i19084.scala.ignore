//> using options -Werror

class Test:
  def t1(y: (
    Int, Int, Int, Int, Int, Int, Int, Int, Int, Int,
    "Bob", Int, 33, Int,
    Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
        ): Unit = y match
    case b @ (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9,
          "Bob", y1, 33, y2,
          z0, z1, z2, z3, z4, z5, z6, z7, z8, z9)
     => // was: !!! spurious unreachable case warning
      ()
    case _ => ()
