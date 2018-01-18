package completeFromSource
import nested._

class Test3 {

  val x = if (true) new B(1) else new C("xx")

  x match {
    case B(n) => println(s"B($n)")
    case C(s) => println(s"C($s)")
  }
}




