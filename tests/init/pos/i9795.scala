class A:
  // Safe initialization check only allows capturing `this` either through primary constructor or synthetic `apply`
  // `Some` case class comes from Scala 2 stdlib, which is not visible, hence the warning
  // For reference:
  // https://github.com/scala/scala3/pull/12711
  // https://github.com/scala/scala3/pull/14283
  val some = Some(this)
