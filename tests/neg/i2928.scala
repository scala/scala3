class Box[T22](var v: T22)

object Test {
 def main(args: Array[String]): Unit = {
   val s = new Box[String]("")
   val i = new Box[Int](3)

   var box: Box[_] = s
   val sv = box.v
   box = i
   box.v = sv // error

   val c: Int = i.v
 }
}
