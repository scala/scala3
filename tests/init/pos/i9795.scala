class A:
  // Safe initialization check only allows capturing `this` either through primary constructor or synthetic `apply`
  // `Some` case class comes from Scala 2 stdlib, which is not visible, hence the warning
  // For reference:
  // https://github.com/lampepfl/dotty/pull/12711
  // https://github.com/lampepfl/dotty/pull/14283
  val some = Some(this)
