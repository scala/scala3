object JSON {
  sealed trait Json
  final case class JArray(elems: JValue*) extends Json
  type JValue = Number | JArray
}

object Main extends App {
  println(JSON.JArray(1))
}
