class Bar extends Ann(1) { // error
  def value = 1
  def annotationType = classOf[Ann]
}

def test =
  // Typer errors
  new Ann // error
  new Ann(1) {} // error
