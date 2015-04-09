package dotty

object language {

  class Feature

  /** Allow higher-kinded type syntax (not yet checked) */
  val higherKinds = new Feature

  /** Keep union types */
  val keepUnions = new Feature

  /** No auto tupling */
  val noAutoTupling = new Feature

}
