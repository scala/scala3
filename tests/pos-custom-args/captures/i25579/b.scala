package p

import language.experimental.captureChecking
import caps.*

trait A extends B:
  def f(): C^ = new C {}
