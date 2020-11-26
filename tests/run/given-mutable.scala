var x = 0
given => Int =
  x += 1
  x
@main def Test =
  assert(summon[Int] == 1)
  assert(summon[Int] == 2)
