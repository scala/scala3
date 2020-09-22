package dotty.communitybuild

object Main {
  /** Builds stdlib.
   *
   *  Output is available in build/pack/lib directory in stdlib project.
   *
   *  In the future, we allow building different projects based on arguments,
   *  but for now stdlib is the only usecase.
   */
  def main(args: Array[String]): Unit =
    projects.stdLib213.publish()
}
