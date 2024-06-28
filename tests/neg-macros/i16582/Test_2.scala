//> using options -explain-cyclic
def test=
  val o1 = ownerWorks(1)
  println(o1)

  val o2 = ownerDoesNotWork(2) // error
  println(o2)
