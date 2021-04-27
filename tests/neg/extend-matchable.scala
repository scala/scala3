trait M0 extends Matchable  // inferred base type is AnyRef
trait M extends Any, Matchable

class C extends Matchable // OK inferred base type is AnyRef
class D(x: Int) extends AnyVal, Matchable // OK
class E extends AnyRef, Matchable // OK
class F extends Any, Matchable // error: Any does not have a constructor

class C1 extends M // OK inferred base type is AnyRef
class D1(x: Int) extends AnyVal, M // OK
class E1 extends AnyRef, M // OK
class F1 extends Any, M // error: Any does not have a constructor

class C2 extends M0 // OK inferred base type is AnyRef
class D2(x: Int) extends AnyVal, M0 // error: illegal trait inheritance
class E2 extends AnyRef, M0 // OK
class F2 extends Any, M0 // error: Any does not have a constructor // error: illegal trait inheritance






