package p

import language.experimental.captureChecking
import caps.*

trait C

trait B:
  def f(): C^
