//> using options -Wsafe-init
class Test extends Macro:
  val abc = nameTuple[Int]

@main
def run(): Unit =
  println(new Test().abc)
