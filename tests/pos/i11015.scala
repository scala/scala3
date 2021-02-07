import annotation.targetName
object Foo:
   def apply[A <: Int]: Int = 0
   @targetName("applyS") def apply[B <: String]: String = "0"

def test = Foo[Int]