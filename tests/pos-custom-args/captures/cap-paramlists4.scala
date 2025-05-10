import language.experimental.captureChecking

trait Foo[U^, V^, W^]:
  type C = {caps.cap}
  type D = {caps.cap}
  type E >: {V,W} <: {U}