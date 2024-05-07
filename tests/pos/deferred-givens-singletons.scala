//> using options -language:experimental.modularity -source future
import compiletime.*

trait A:
  type Elem: Singleton

class B extends A:
  type Elem = 1

class C[X: Singleton] extends A:
  type Elem = X


