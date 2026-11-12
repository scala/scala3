import language.experimental.captureChecking
trait P:
  self: P { def m(): () => Int } =>
trait A extends P:  // error
  self: A { def m(): () ->{caps.fresh} Int } =>
trait B extends P:  // error
  self: B { def m(): () ->{caps.fresh.rd} Int } =>
