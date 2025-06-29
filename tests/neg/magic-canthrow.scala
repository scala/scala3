import language.experimental.erasedDefinitions
import java.io.IOException

class CanThrow[-E <: Exception]

def foo[E <: Exception](e: E)(using erased CanThrow[E]): Nothing = throw e

erased def magic[E]: E = magic  // error

def Test = foo(new IOException)(using magic)

