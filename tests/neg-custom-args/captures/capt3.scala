import annotation.retains
class C
type Cap = C @retains[caps.any.type]

def test1() =
  val x: Cap = C()
  val y = () => { x; () }
  val z = y
  z: (() -> Unit) // error

def test2() =
  val x: Cap = C()
  def y = () => { x; () }
  def z = y
  z: (() -> Unit) // error

def test3() =
  val x: Cap = C()
  def y = () => { x; () }
  val z = y
  z: (() -> Unit) // error

def test4() =
  val x: Cap = C()
  val y = () => { x; () }
  def z = y
  z: (() -> Unit) // error
