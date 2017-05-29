abstract class AA

trait TT

class A { self: B =>

}

@scala.annotation.Annotation class C // error

class B extends A() {
}

object Test {

  @scala.annotation.Annotation type T = String // error
  @scala.annotation.Annotation val x = 1 // error
  @scala.annotation.Annotation def f = 1 // error

  (1: @scala.annotation.Annotation) // error


  new AA // error

  new TT // error

  new A // error "A does not conform to its self type B; cannot be instantiated"

// the following are OK in Typer but would be caught later in RefChecks

  new A() {}

  new AA() {}

  object O extends A

  object OO extends AA
}

