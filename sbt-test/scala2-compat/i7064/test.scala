//> using dep com.lihaoyi:fastparse_2.13:2.1.3

import fastparse._

def foo[$: P] = P(StringIn("foo") | AnyChar)
def bar[$: P] = P("bar")
