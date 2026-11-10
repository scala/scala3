package first

class LetItCrash(x: Int) :
  def this(x: Int, z: String) =
    this(x)
  Static.callInfo()

@main
def main(): Unit =
  val _ = new LetItCrash(42,"FOO")
