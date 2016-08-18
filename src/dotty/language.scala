package dotty

object language {

  class Feature

  /** Keep union types */
  val keepUnions = new Feature

  val f = "".contains("", (_: Int))

}
