import language.experimental.captureChecking

trait Foo[U^, V^, W^]:
  type C = {caps.any}
  type D = {caps.any}
  type E >: {V,W} <: {U}