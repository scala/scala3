class IdAndMsg(val id: Int,  val msg: String = "")

case object ObjA extends IdAndMsg(1)     // error
case object ObjB extends IdAndMsg(2)

object IdAndMsg {                        // error
  val values = List(ObjA , ObjB)
}

object Test {
  def main(args: Array[String]): Unit = {
    ObjA
    println(IdAndMsg.values)
  }
}