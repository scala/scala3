// The SAM types are loaded from TASTy
@main def Test =
  val f1: [A] => MySam[A] = [A] => (x: A) => x
  assert(f1[Int](1) == 1)

  val f2: [A] => IntSam = [A] => (x: Int) => x + 1
  assert((f2[String]: GenSam[Int])(1) == 2)

  val f3: [A] => WithHelper[A] = [A] => (x: A) => x.toString.length
  assert(f3[Int](100) == 3 && f3[Int].twice(2) == 4)
