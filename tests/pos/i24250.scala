
trait Foo
val foo =
  new Foo:
    // comment
  end new
val foo2 =
  new Foo:
  end new
val foo3 =
  new Foo {
  }
  end new

class C
val c =
  new C:
  end new
val c2 =
  new C {
  }

class D:
end D
val d =
  new D:
    def more = ???
  end new
