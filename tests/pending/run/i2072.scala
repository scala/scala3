trait T { lazy val x: String = "foo" }
trait U { def x: Any }
abstract class AC extends T with U // { def x: Any }

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
