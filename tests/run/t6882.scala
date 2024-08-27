//> using options --Xtarget:11
// using jvm 11+
// test: -jvm 11+
// scalajs: --skip

class C private (private val i: Int, private val j: Int) {
  private val c = i + C.secret

  @inline def f = j * 2
}
object C {
  def unwrap(c: C): Int = c.c

  def apply(i: Int, j: Int): C = new C(i, j)

  private def secret = 5
}

class D(d0: String) {
  private def d = d0
  def e = new E
  class E {
    def e = D.this.d
  }
}
object D {
}

object Top {
  private def i = 42
  class Nested {
    def f = i
  }
  def j = new Nested().f
}

class TopHeavy {
  private def i = TopHeavy.underlying
}
object TopHeavy {
  private def underlying = 42
  class Nested {
    def f = new TopHeavy().i
  }
  def j = new Nested().f
}

class TippyTop {
  private def secret = 42
  class Medial {
    class Nadir {
      class Abyssal {
        def answer = secret
      }
    }
  }
}

object Test {
  import java.lang.reflect.Modifier.{PRIVATE => Private}
  def main(args: Array[String]): Unit = {
    assert(C.unwrap(C(42, 27)) == 47)
    for (m <- Class.forName("C$").getDeclaredMethods; n = m.getName if n.contains("secret")) {
      assert(n == "secret", s"name was $n")
      assert((m.getModifiers & Private) != 0, s"$n not private")
    }
    assert(new D("mystery").e.e == "mystery")
    for (m <- Class.forName("D").getDeclaredMethods; n = m.getName if n.contains("d")) {
      assert(n == "d")
      assert((m.getModifiers & Private) != 0)
    }
    assert(Top.j == 42)
    for (m <- Class.forName("Top$").getDeclaredMethods; n = m.getName if n.endsWith("i")) {
      assert(n == "i", s"method $n != i")
      assert((m.getModifiers & Private) != 0)
    }
    assert(TopHeavy.j == 42)
    for (m <- Class.forName("TopHeavy$").getDeclaredMethods; n = m.getName if n.contains("underlying")) {
      assert(n == "underlying", s"method $n != underlying")
      assert((m.getModifiers & Private) != 0)
    }
    assert({val x = TippyTop(); val y = x.Medial(); val z = y.Nadir(); z.Abyssal().answer} == 42)
    for (m <- Class.forName("TippyTop").getDeclaredMethods; n = m.getName if n.contains("secret")) {
      assert(n == "secret", s"method $n != secret")
      assert((m.getModifiers & Private) != 0)
    }
  }
}
