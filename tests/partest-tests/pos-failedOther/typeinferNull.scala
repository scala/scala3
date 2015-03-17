object typeinferNull {

  val x1 = null :: Nil
  val x2 = List(null)

  var y1: List[Null] = x1
  y1 = x2

}
