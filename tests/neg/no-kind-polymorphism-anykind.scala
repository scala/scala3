// scalac: -Yno-kind-polymorphism

trait Foo[T <: AnyKind] // error: Not found: type AnyKind
