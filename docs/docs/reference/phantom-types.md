---
layout: doc-page
title: "Phantom Types"
---


What is a phantom type?
-----------------------

A phantom type is a manifestation of abstract type that has no effect on the runtime. 
These are useful to prove static properties of the code using type evidences. 
As they have no effect on the runtime they can be erased from the resulting code by 
the compiler once it has shown the constraints hold.

When saying that they have no effect on the runtime we do not only mean side effects 
like IO, field mutation, exceptions and so on. We also imply that if a function receives 
a phantom its result will not be affected by this argument.

As phantoms do not live at runtime they cannot be subtypes of `scala.Any`, which defines
methods such as `hashCode`, `equals`, `getClass`, `asInstanceOf` and `isInstanceOf`. 
All these operations cannot exist on phantoms as there will not be an underlying object 
instance at runtime. At first glance this could look like a limitation, but in fact not 
having `asInstanceOf` will make constraints more reliable as it will not be possible to 
downcast a phantom value to fake an evidence.

If they don't live in the universe bounded by `scala.Any` and `scala.Nothing` where do 
they live? The answer is in their own type universes bounded by their phantom `Any` and `Nothing`. 
In fact we allow multiple phantom universes to exist.

```none
          +-----+                     +---------------+           +--------------------+
          | Any |                     | MyPhantom.Any |           | MyOtherPhantom.Any |
          +-----+                     +---------------+           +--------------------+
             |                               |                              |
        +----+------+                  +-----+------+                      ...
        |           |                  |            |
   +--------+   +--------+          +------+   +--------+
   | AnyRef |   | AnyVal |          | Inky |   | Blinky |
   +--------+   +--------+          +------+   +--------+
      ...          ...                  |           |
    +------+        |               +-------+       |
    | Null |        |               | Pinky |       |
    +------+        |               +-------+       |
       |            |                   |           |
       +------+-----+                   +----+------+                      ...
              |                              |                              |
         +---------+                +-------------------+       +------------------------+
         | Nothing |                | MyPhantom.Nothing |       | MyOtherPhantom.Nothing |
         +---------+                +-------------------+       +------------------------+
```

Inside a universe the full Dotty type system is supported. But we cannot mix types from
different universes with `&`, `|` or in type bounds. Each type must be fully defined in a single universe.


Implement your own phantom type
-------------------------------
Phantom types are defined by an `object` extending `scala.Phantom`. This object will represent
a universe of phantom types that is completely separated from types in `scala.Any` or other 
phantom universes. We can define our phantom universe `MyPhantoms`.

```scala
object MyPhantoms extends Phantom
```

```scala
package scala
trait Phantom { // only an `object` can extend this trait
  protected final type Any // not a subtype of scala.Any
  protected final type Nothing // subtype of every subtype of this.Any
  protected final unused def assume: this.Nothing
}
```

The types in the phantom universe are defined by the top type `Phantom.Any` and bottom type 
`Phantom.Nothing`. This means that `MyPhantoms.Any` and `MyPhantoms.Nothing` are the bounds 
of the phantom types in `MyPhantoms`, these bounds are `protected` and can not be accessed 
from outside `MyPhantoms` unless an alias is defined for them.

New phantom types can be defined using `type XYZ <: OtherPhantom` (where `>: MyPhantom.Nothing` 
will be inferred), this would be the equivalent of `class XYZ extends OtherClass` on types
only (no runtime definitions). Or aliased with `type MyAny = OtherPhantom`. Within `MyPhantoms` 
it is possible to refer to `MyPhantoms.Any` and `MyPhantoms.Nothing` with `this.Any` and 
`this.Nothing` (or just `Any` and `Nothing` but not recommended). Using this we will define
the four phantoms: `Inky`, `Blinky`, `Pinky` and `Clyde`.

```scala
object MyPhantoms extends Phantom {
  type Inky <: this.Any
  type Blinky <: this.Any
  type Pinky <: Inky
  type Clyde <: Pinky
}
```

Values of phantom type can be created using the `protected def assume`. This value can be 
used as a value of this phantom type as its type is `this.Nothing` (or `MyPhantoms.Nothing`).
Usually this value will be used to define a `implicit def` that returns the phantom with a more 
precise type. In our example we will only create values of type `Pinky` and `Clyde`

```scala
object MyPhantoms extends Phantom {
  ... // Type definition

  unused def pinky: Pinky = assume
  unused def clyde: Clyde = assume
}
```

### Using the phantoms

For the user of the phantom type there is almost no difference, except for stronger type guarantees. 
We can look at the following simple application:

```scala
import MyPhantoms._
object MyApp {
  def run(unused phantom: Inky) = println("run")
  def hide(unused phantom: Blinky) = println("run")

  run(pinky)
  run(clyde)
}
```

Note that given the way we defined the phantoms it is impossible to call `hide` as we did not 
expose any value of type `Blinky` to the user. We cannot call `hide(MyPhantoms.assume)` as 
`assume` is protected, `hide(null.asInstanceOf[Blinky])` does not compile because it is impossible 
to cast to a phantom and `hide(throw new Exception)` (or `hide(???)`) does not compile as `throw` of
type `scala.Nothing` is not in the same type universe as `Blinky`. Good, we caught all possible 
mistakes before when compiling the code, no surprises at runtime (hopefully not in production).


What happens with Phantoms at runtime?
--------------------------------------

All phantoms are erased using the `unused` keyword. They will simply not exist at runtime.
