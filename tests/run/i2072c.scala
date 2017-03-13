trait T { lazy val x: String = "foo" }
trait U { def x: Any }
class AC extends T with U // { def x: Any }

abstract class B { def x: Any }
class C extends B { def x: String = "abc" }

package p2 {
trait T1 { def f: Any }
trait T2 extends T1 { def f: Number = ??? }
trait T3 extends T1 { override def f: Integer = ??? }
class C extends T2 with T3
}

package p3 {

  trait A { def f: Any }
  trait B extends A { def f: String }
  class C extends B { def f = "abc" }

}

object Test {
  def main(args: Array[String]): Unit = {
     val ac = new AC
     ac.x
     (ac: T).x
     (ac: U).x
     (new C).x
     ((new C): B).x
     val p2c = new p2.C
     p2c.f
     (p2c: p2.T1).f
     (p2c: p2.T2).f
     (p2c: p2.T3).f
     val p3c = new p3.C
     p3c.f
     (p3c: p3.A).f
     (p3c: p3.B).f
  }
}
