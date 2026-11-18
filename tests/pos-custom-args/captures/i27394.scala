import language.experimental.captureChecking
def coerce[T, U](x: T & U): T & U = x
def coerceOr[T, U](x: T | U): T | U = x
def good1[A^, B^, X^ <: {A, B}](f: () ->{X} Unit): () ->{X} Unit =
  coerce[() ->{X} Unit, () ->{A, B} Unit](f)
def good2[A^, B^, X^ <: {A, B}](f: () ->{X} Unit): () ->{X} Unit =
  val g: (() ->{X} Unit) & (() ->{A, B} Unit) = f
  g
def good3(a: Object^, b: Object^, f: () ->{a, b} Unit): () ->{f} Unit =
  coerce[() ->{f} Unit, () ->{a, b} Unit](f)
def good4(a: Object^, b: Object^, f: () ->{a} Unit): () ->{a, b} Unit =
  coerceOr[() ->{a} Unit, () ->{b} Unit](f)
