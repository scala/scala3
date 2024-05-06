//> using options -Xprint:typer -language:experimental.modularity -source future

def f1[S, T <: S : Singleton](x: S) = ()
def f2[S, T >: S : Singleton](x: S) = ()

def Test =
  f1(42) // f1[Int, Singleton & Int] // should infer (42 : Int) or throw an error?
  f2(42) // f2[(42 : Int), (42 : Int)]