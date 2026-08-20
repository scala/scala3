// https://github.com/scala/scala3/issues/13468
// Second compilation round (the reporter's "uncomment the line and recompile"):
// `container`'s type and `MyOpaque`'s opaque parameter must come back from TASTy
// as `Container[8]`, not `Container[Int]`, for this `summon` to resolve.
val shouldWork = summon[container.type <:< Container[8]]
