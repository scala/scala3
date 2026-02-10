package scala3build

import mill.api.Cross

enum BuildType:
  case Bootstrapping
  case Final

  def asString: String = this match {
    case Bootstrapping => "bootstrapping"
    case Final => "final"
  }

  def isBootstrapping: Boolean =
    this == Bootstrapping

object BuildType:
  lazy val allValues = Seq[BuildType](
    BuildType.Bootstrapping,
    BuildType.Final
  )

  given Cross.ToSegments[BuildType] =
    new Cross.ToSegments(buildType => List(buildType.asString))
