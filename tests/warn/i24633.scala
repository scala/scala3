//> using options -Werror -Wunused:imports

object foo {
  val min = 0
  val max = 1
}

def test(max: Int) = {
  import foo.{max as _, *}
  s"$min - $max"
}

def local =
  val max = 42
  import foo.{max as _, *}
  s"$min - $max"

class Limit(max: Int):
  import foo.{max as _, *}
  def test = s"$min - $max"
