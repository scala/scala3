package scala3build

import mill.*

trait CrossScala3Module extends Cross.Module[BuildType] with Scala3Module {
  def buildType = crossValue
}
