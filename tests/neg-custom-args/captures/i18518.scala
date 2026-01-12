import language.experimental.captureChecking
type Foo1 = [R] -> (x: Unit) ->{} Unit  // error
type Foo2 = [R] -> (x: Unit) ->{any} Unit  // error
type Foo3 = (c: Int^) -> [R] -> (x: Unit) ->{c} Unit  // error
type Foo4 = (c: Int^) -> [R] -> (x0: Unit) -> (x: Unit) ->{c} Unit
