abstract class AA

trait TT

class A { self: B =>

}

@scala.annotation.Annotation class C // error

class B extends A() {
}

object Test {

  @scala.annotation.Annotation type T = String // ok, annotations do not count as new
  @scala.annotation.Annotation val x = 1 // ok
  @scala.annotation.Annotation def f = 1 //ok

  (1: @scala.annotation.Annotation) // ok


  new AA // error

  new TT // error

  new A // error

// the following are OK in Typer but would be caught later in RefChecks

  new A() {}

  new AA() {}

  object O extends A

  object OO extends AA
}
