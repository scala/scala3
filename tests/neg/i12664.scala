trait Step {
  type Self
  type Next[A]
}

trait DynamicNextStep {
  type OneOf[Self, Next[_]]
  def apply(s: Step): OneOf[s.Self, s.Next]
}

object X extends DynamicNextStep {
  override type OneOf[Self] = Self  // error
  override def apply(s: Step) = ???
}
