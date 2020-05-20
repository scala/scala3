trait Cons[X, Y]

def solve[X, Y](using Cons[X, Y] =:= Cons[1, Cons[2, Y]]) = ()

@main def main = solve // error
