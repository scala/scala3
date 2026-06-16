class Box[A](val elem: A)
class CoBox[+A](val elem: A)

def applyAll[A](fs: Box[A => Unit], x: A): Box[() ->{fs*} Unit] =
  Box(() => fs.elem(x))

def applyAllCo[A](fs: CoBox[A => Unit], x: A): CoBox[() ->{fs*} Unit] =
  CoBox(() => fs.elem(x))

// Same with inferred result types
def test =
  def applyAll[A](fs: Box[A => Unit], x: A) =
    Box(() => fs.elem(x))

  def applyAllCo[A](fs: CoBox[A => Unit], x: A) =
    CoBox(() => fs.elem(x))

