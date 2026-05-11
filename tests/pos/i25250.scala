//> using options -Wall -Werror

@main def test = {
  println(Seq(res1, res2, res3, res4, res5).mkString("\n"))
}

def fooB(b: Int) = b * b
def fooC(c: Int) = c * c * c

// compiles
val res1 = fooB(
  b =
    fooC:
      2
    + 1
)
val res2 = fooB:
  fooC:
    2
  + 1
val res3 = fooB(
    fooC(2)
    + 1
)

// doesn't compile
val res4 = fooB(
  fooC:
    2
  + 1
)
val res5 = fooB(
  b = fooC:
    2
  + 1
)
