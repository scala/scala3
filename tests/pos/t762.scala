trait Foo { type T }
trait Bar1 extends Foo { val x : Foo { type T <: Bar1.this.T } = this }
trait Bar2 extends Foo { val x : Foo { type T =  Bar2.this.T } = this }
trait Bar3 extends Foo { val x : Foo { type T >: Bar3.this.T } = this }
