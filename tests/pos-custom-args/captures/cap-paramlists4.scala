import language.experimental.captureChecking

trait Foo[cap U,V,W]:
  cap C = caps.cap
  cap D = {caps.cap}
  cap E >: {V,W} <: U