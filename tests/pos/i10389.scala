import scala.util._

object FooBar {
  def foo = List("1","two","3").collect{ x => 
    Try(x.toInt) match { 
      case Success(int) => int 
    }
  }
}
