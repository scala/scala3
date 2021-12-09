class Outer {
  class Inner
}
@main def Test =
  assert(
    classOf[Outer#Inner]
      .getDeclaredFields
      .filter(_.getName == "$outer")
      .exists(_.isSynthetic))
