import java.io.IOException

class CanThrow[-E <: Exception]

def foo[E <: Exception](e: E)(using erased CanThrow[E]): Nothing = throw e

erased def magic[E]: E = magic // error
inline def moreMagic[E]: E = moreMagic

def Test =
  foo(new IOException)(using magic)
  foo(new IOException)(using moreMagic) // should be error
