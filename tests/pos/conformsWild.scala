object Test {

  val x: Function1[_, _] = (x: String) => 1

  val y: Function1[_, _] = x => 1
  val y0: Function1[_, _] = x => x
  val y1: Function1[_, Nothing] = x => x

  val z: (_, _) = (1, 2)

}
