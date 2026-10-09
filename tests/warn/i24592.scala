//> using options -Wall

def i24592a =
  val transforms: Seq[(String, Int)] = Nil
  transforms.map((str, int) => str) // warn untupled param not local

def i24592b =
  val transforms: Seq[(String, Int)] = Nil
  transforms.map((str, _int) => str) // no warn
