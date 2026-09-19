//> using options -Wunused:params
import scala.annotation.unused
class A {
  def f(@unused inUse: Int):Int = inUse // warn
}