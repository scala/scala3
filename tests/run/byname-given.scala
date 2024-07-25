//> using options -language:experimental.modularity -source future

@main def Test =
  var x: Int = 0
  given () => Int = x
  assert(summon[Int] == 0)
  x += 1
  assert(summon[Int] == 1)

