import language.experimental.captureChecking
import caps.any
type Foo1 = [R] -> (x: Unit) ->{} Unit
type Foo2 = [R] -> (x: Unit) ->{any} Unit
type Foo3 = (c: Int^) -> [R] -> (x: Unit) ->{c} Unit  // error
type Foo4 = (c: Int^) -> [R] -> (x0: Unit) -> (x: Unit) ->{c} Unit // error
