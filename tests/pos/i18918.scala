
trait SuperTrait {
	sealed trait InnerTrait
	case class Foo() extends InnerTrait
}

trait OtherTrait

trait TraitWithSelfType extends SuperTrait { this: OtherTrait =>
	summon[deriving.Mirror.Of[Foo]]
	summon[deriving.Mirror.Of[InnerTrait]]
}

object Implementation extends TraitWithSelfType, OtherTrait {
	summon[deriving.Mirror.Of[Foo]]
	summon[deriving.Mirror.Of[InnerTrait]]
}
