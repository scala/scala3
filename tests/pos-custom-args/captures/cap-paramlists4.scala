import language.experimental.captureChecking

trait Foo[cap U, cap V, cap W]:
  cap type C = {caps.cap}
  cap type D = {caps.cap}
  cap type E >: {V,W} <: {U}