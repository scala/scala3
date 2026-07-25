package scala3build

/**
 * Enum for the two stages to build compiler-related JARs: non-bootstrapped and bootstrapped
 */
sealed abstract class Mode(val name: String)

object Mode:
  /**
   * First build of a module, using an already published Scala 3 version
   */
  case object NonBootstrapped extends Mode("non-bootstrapped")

  /**
   * Second (and last) build of a module, using newly built compiler and standard library
   *
   * This uses the standard library and compiler built in the NonBootstrapped stage, and
   * the scaladoc built in the Bootstrapped stage (in this stage, we build scaladoc and use it
   * straightaway in the same stage).
   */
  case object Bootstrapped extends Mode("bootstrapped")

  lazy val allValues = Seq[Mode](
    NonBootstrapped,
    Bootstrapped
  )
