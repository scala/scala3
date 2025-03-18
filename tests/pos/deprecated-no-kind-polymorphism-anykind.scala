// This test is kept as a placeholder for historical reasons.
// The -Yno-kind-polymorphism flag is now deprecated and has no effect.
// Kind polymorphism with AnyKind is always enabled.

trait Foo[T <: AnyKind] // This now works as AnyKind is always defined
