inline trait A(i: Int):
  val x = i
  val y = i

class B(i: Int) extends A(i)

@main def Test =
  var c = 0

  val b1 = new B({
    for i <- 0 to c
    do println(i)
    c += 1
    c
  })
  val b2 = new B({
    for i <- 0 to c
    do println(i)
    c += 1
    c
  })
  val b3 = new B({
    for i <- 0 to c
    do println(i)
    c += 1
    c
  })