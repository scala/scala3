trait T {

 val f1: Int = {println("T.f1"); -1}
 val f2: Int = {println("T.f2"); -2}
 val f3: Int = {println("T.f3"); -3}
 val f4: Int = {println("T.f4"); -4}

 println(s"$f1 $f2 $f3 $f4")
}

trait U {
  val f2: Int
}

class Test0 extends U {
  final val f1 = 1
  final val f2 = 2
  final val f3 = f1 + f2
  val f4: 3 = f3
}

class Test1 extends U {
  final val f1 = 1
  final val f3 = f1 + f2
  final val f2 = 2
  val f4: 3 = f3


}

class Test extends T {
 override final val f1 = /*super.f1*/ 1 + f2  // warn
 override final val f2 = 2                    // warn
 override final val f3 = {println(3); 3}      // warn
 override val f4 = f3 + 1                     // warn

 def g: 3 = { println("g"); 3 }
 final val x = g + 1
 def main(args: Array[String]): Unit = {
   new Test0
   new Test1
 }
}
