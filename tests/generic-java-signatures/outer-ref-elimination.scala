// This test checks that references to outer classes in inner classes are
// eliminated in some cases when they are not used. This is done in the
// ExplicitOuter phase. See issue #19569 for discussions.

object helper {
  def test(cls: Class[?]) = println(cls.getDeclaredConstructors.toList)
}

import helper.test

object T1 { class C1; test(classOf[C1]) }
object T2 { new AnyRef { class C2; test(classOf[C2]) } }
object T3 { def t3(): Unit = { class C3; test(classOf[C3]) } }
object T4 { def t4(): Unit = new AnyRef { class C4; test(classOf[C4]) } }

// The outer reference in C5 is not eliminated because C5 is publicly
// accessible as a member of T5. Therefore, its constructor needs to conform
// to the expected signature, with the outer reference parameter.
class T5 { class C5; test(classOf[C5]) }

class T6 { new AnyRef { class C6; test(classOf[C6]) } }
class T7 { def t7(): Unit = { class C7; test(classOf[C7]) } }
class T8 { def t8(): Unit = new AnyRef { class C8; test(classOf[C8]) } }

// Here, the outer reference in C9 is not eliminated because C9 needs to access
// the field x.
class T9 { var x = 451; def t9(): Unit = { class C9 {def getX = x}; test(classOf[C9])} }

object Test {
  def main(args: Array[String]): Unit = {
    T1
    T2
    T3.t3()
    T4.t4()
    new T5()
    new T6()
    new T7().t7()
    new T8().t8()
    new T9().t9()
  }
}
