// scalajs: --skip
// (JVM-only reflection-based test)

class Outer:
  var x = 0
  class Inner:
    def f(): Unit = Outer.this.synchronized { x += 1 }
  def own(): Unit = this.synchronized { x += 1 }
  def ownImplicit(): Unit = synchronized { x += 1 }

object Test:
  def main(args: Array[String]): Unit =
    val o = new Outer()
    val i = new o.Inner()

    val om = o.getClass.getDeclaredMethods.find(_.getName == "own").get
    println(om.getModifiers & java.lang.reflect.Modifier.SYNCHRONIZED)

    val om2 = o.getClass.getDeclaredMethods.find(_.getName == "ownImplicit").get
    println(om2.getModifiers & java.lang.reflect.Modifier.SYNCHRONIZED)

    val fm = i.getClass.getDeclaredMethods.find(_.getName == "f").get
    println(fm.getModifiers & java.lang.reflect.Modifier.SYNCHRONIZED)

