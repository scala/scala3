class IdAndMsg(val id: Int,  val msg: String = "")

case object ObjA extends IdAndMsg(1)     
case object ObjB extends IdAndMsg(2)

object IdAndMsg {                        
  val values = List(ObjA , ObjB)
}

object Test {
  def main(args: Array[String]): Unit = {
    ObjA
    println(IdAndMsg.values)
  }
}
// nopos-error: No warnings can be incurred under -Werror.