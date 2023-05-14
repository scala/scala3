import scala.deriving.Mirror
// Works:
sealed trait HasFoo { sealed trait Foo; case object B extends Foo; summon[Mirror.SumOf[Foo]] }

// Doesn't work:
final class UsesFoo extends HasFoo { summon[Mirror.SumOf[Foo]] }