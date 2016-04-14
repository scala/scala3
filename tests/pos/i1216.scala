object Main {
  val MAX = 10
  val s1, s2, target = new Array[Long](MAX)

  var i, j = 0

  while (i < MAX) {
    target(i) = s1(i) + s2(i)
    i+= 1
  }
}
