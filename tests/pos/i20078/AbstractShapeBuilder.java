public abstract class AbstractShapeBuilder<B extends AbstractShapeBuilder<B, S>, S extends Shape> {
	abstract public B addTrait(Trait trait);
}