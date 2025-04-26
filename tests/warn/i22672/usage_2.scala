
//> using options -deprecation

package q

def test = p.f // inline f is nowarn

def bodily = p.body // transparent inline with annotated body

@deprecated("do I even know how it works", since="0.1")
def huh = "hello"

def failing = huh // warn
