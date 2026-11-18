import language.experimental.captureChecking
def coerce[T, U](x: T & U): T & U = x
def coerceOr[T, U](x: T | U): T | U = x
def bad[A^, B^, X^ <: {A, B}](f: () ->{X} Unit): () -> Unit =
  coerce[() ->{X} Unit, () ->{A, B} Unit](f)   // error
def bad2[A^, B^, X^ <: {A, B}](f: () ->{X} Unit): () -> Unit =
  val g: (() ->{X} Unit) & (() ->{A, B} Unit) = f
  g // error
def bad3(a: Object^, b: Object^, f: () ->{a} Unit): () ->{a} Unit =
  coerceOr[() ->{a} Unit, () ->{b} Unit](f) // error
