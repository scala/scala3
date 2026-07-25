package scala3build

import mill.*

trait CrossHelper[T] extends Cross.Module[Mode] { self: T =>
  def crossValue = Mode.Bootstrapped

  def `non-bootstrapped`: T & CrossHelper.NonBootstrapped

  def apply()(implicit mode: Mode): T =
    mode match {
      case Mode.NonBootstrapped => `non-bootstrapped`
      case Mode.Bootstrapped => self
    }
}

object CrossHelper {
  trait NonBootstrapped extends Cross.Module[Mode] {
    def crossValue = Mode.NonBootstrapped
    def moduleDir: os.Path = super.moduleDir / os.up
  }
}
