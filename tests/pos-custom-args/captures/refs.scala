type Proc = () => Unit

class MonoRef(init: Proc):
  type MonoProc = Proc
  var x: MonoProc = init
  def getX: MonoProc = x
  def setX(x: MonoProc): Unit = this.x = x

def test(p: Proc) =
  val x = MonoRef(p)
  x.setX(p)
  val y = x.getX
  val yc1: Proc = y
  val yc2: () ->{x} Unit = y
  val yc3: () ->{cap[test]} Unit = y

class MonoRef2(init: () => Unit):
  var x: () ->{cap[MonoRef2]} Unit = init
  def getX: () ->{cap[MonoRef2]} Unit = x
  def setX(x: () ->{cap[MonoRef2]} Unit): Unit = this.x = x

def test2(p: Proc) =
  val x = MonoRef2(p)
  x.setX(p)
  val y = x.getX
  val yc1: Proc = y
  val yc2: () ->{x} Unit = y
  val yc3: () ->{cap[test2]} Unit = y

