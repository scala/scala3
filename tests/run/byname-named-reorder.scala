def consume(a: => Int, b: Int, c: Int = 0): Int =
  println(s"consume b=$b c=$c")
  val first = a
  val second = a
  println(s"a1=$first")
  println(s"a2=$second")
  first + second + b + c

@main def Test: Unit =
  var state = 0

  def byNameArg: Int =
    println(s"byName sees $state")
    state

  def strictB: Int =
    state = 1
    println("strict b runs")
    10

  def strictC: Int =
    state = 2
    println("strict c runs")
    20

  val result = consume(c = strictC, a = byNameArg, b = strictB)
  println(s"result=$result")
