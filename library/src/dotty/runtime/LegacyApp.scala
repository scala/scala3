package dotty.runtime

/** Replaces the `scala.App` class which relies on `DelayedInit` functionality,
  * not supported by Dotty.
  */
class LegacyApp {
  def main(args: Array[String]): Unit = ()
}
