object Test {

  val x: Function1[?, ?] = (x: String) => 1

  val y: Function1[?, ?] = x => 1
  val y0: Function1[?, ?] = x => x
  val y1: Function1[?, Nothing] = x => x

  val z: (?, ?) = (1, 2)

}
