trait Converter[A, B] {
  def convert: A => B
}

inline given Converter[Int, String] = new Converter { // warn
  def convert = _.toString()
}

def foo(using bar: Converter[Int, String]) =
  "foo"

@main
def main =
  foo
  foo
  foo
  foo