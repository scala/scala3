//> using options -experimental

import test.MyMacro

trait Command {
  def run(): Int
  def run2(foo: String): Int
  def run3: Int
}

@main
def Test = {
  val myCommand: Command = MyMacro.client[Command, Int](() => 12)
  println(myCommand.run()) // 12
  println(myCommand.run2("test")) // 12
  println(myCommand.run3) // 12
}
