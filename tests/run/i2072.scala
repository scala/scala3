trait T { 
  def m = "" ; 
  val v = "" ; 
  lazy val lv = ""; 
  object o
}

// deferred methods always lose against concrete ones, so none of these declarations actually survive in bytecode
// the mixed in members from T take precedence in the linearisation order
abstract class AC extends T {
  def m: Any
  def v: Any
  def lv: Any
  def o: Any
}

class C extends AC{}

object Test {
 def main(args: Array[String]): Unit = {
   val c = new C
   c.m
   c.v
   c.lv
   c.o
 }
} 

