trait Bundle extends reflect.Selectable

trait HasFoo { def foo = 23 }

@main def Test: Unit =
  val foo = new Bundle { object Foo extends HasFoo }
  val bar = new Bundle { object Bar extends Bundle { def bar = 47 } }
  val baz = new Bundle {
    class Xyz extends Bundle { def xyz = 31 }
    val ref: Xyz = Xyz()
  }
  val qux = new Bundle {
    object Deferred {
      class Qux extends Bundle {
        def qux = 71
      }
    }
    val Qux = Deferred.Qux()
  }
  val quux = new Bundle {
    object Deferred extends Bundle {
      object Quux extends Bundle {
        def quux = 93
      }
    }
  }
  assert(foo.Foo.foo == 23)
  assert(bar.Bar.bar == 47)
  assert(baz.ref.xyz == 31)
  assert(qux.Qux.qux == 71)
  assert(quux.Deferred.Quux.quux == 93)
