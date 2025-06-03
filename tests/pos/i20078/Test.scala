@main def Test =
  val builder: AbstractShapeBuilder[? <: AbstractShapeBuilder[?, ?], ? <: Shape] = ???
  List.empty[Trait].foreach(builder.addTrait(_))