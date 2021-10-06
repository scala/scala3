trait Foo extends reflect.Selectable

val f: Foo { var i: Int } = new Foo { var i: Int = 0 } // error

val f2: Foo { val i: Int; def i_=(x: Int): Unit } = new Foo { var i: Int = 0 } // OK
