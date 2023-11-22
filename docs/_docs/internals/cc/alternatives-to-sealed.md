A capture checking variant
==========================

 - Our starting point is the currently implemented system, where encapsulation is achieved by disallowing root capabilities in
 the types of sealed type variables. By contrast no restrictions apply
 to boxing or unboxing.

 - We now treat all type variables as sealed (so no special `sealed` modifier is necessary anymore). A type variable cannot be instantiated to a type that contains a covariant occurrence of `cap`. The same restriction applies to the types of mutable variables and try expressions.

 - For any immutable variable `x`, introduce a _reach_ capability `x*` which stands for
   "all capabilities reachable through `x`". We have `{x} <: {x*} <: dcs(x)}` where the deep capture set `dcs(x)` of `x`
   is the union of all capture sets that appear in covariant position in the type of `x`. If `x` and `y` are different
   variables then `{x*}` and `{y*}` are unrelated.

 - We modify the VAR rule as follows:

        x: T in E
        -----------
        E |- x: T'

   where T' is T with (1) the toplevel capture set replaced by `{x}` and
   (2) all covariant occurrences of cap replaced by `x*`, provided there
   are no occurrences in `T` at other variances. (1) is standard,
   whereas (2) is new.

- Why is this sound? Covariant occurrences of cap must represent capabilities that are reachable from `x`, so they are included in the meaning of `{x*}`. At the same time, encapsulation is still maintained since no covariant occurrences of cap are allowed in instance types of
type variables.

## Examples:

Assume
```scala
type Proc = () => Unit

class Ref[T](init: T):
  private var x: T = init
  def get: T = x
  def set(y: T) = { x = y }
```
Note that type parameters no longer need (or can) be annotated with `sealed`.

The following example does not work.
```scala
def runAll(xs: List[Proc]): Unit =
  var cur: List[Proc] = xs // error: Illegal type for var
  while cur.nonEmpty do
    val next: () => Unit = cur.head
    next()
    cur = cur.tail

  usingFile: f =>
    cur = ((() => f.write()): (() ->{f*} Unit)) :: Nil
```
Same with refs:
```scala
def runAll(xs: List[Proc]): Unit =
  val cur = Ref[List[Proc]](xs) // error, illegal type for type argument to Ref
  while cur.get.nonEmpty do
    val next: () => Unit = cur.get.head
    next()
    cur.set(cur.get.tail: List[Proc])

  usingFile: f =>
    cur.set:
      (() => f.write(): () ->{f*} Unit) :: Nil
```

The following variant makes the loop typecheck, but
still rejects the incorrect leakage in `usingFile`.
```scala
def runAll(xs: List[Proc]): Unit =
  var cur: List[() ->{xs*} Unit] = xs  // OK, by revised VAR
  while cur.nonEmpty do
    val next: () ->{xs*} Unit = cur.head
    next()
    cur = cur.tail: List[() ->{xs*} Unit]

  usingFile: f =>
    cur = (() => f.write(): () ->{f*} Unit) :: Nil // error since {f*} !<: {xs*}
```

Same with refs:
```scala
def runAll(xs: List[Proc]): Unit =
  val cur = Ref[List[() ->{xs*} Unit]](xs)  // OK, by revised VAR
  while cur.get.nonEmpty do
    val next: () ->{xs*} Unit = cur.get.head
    next()
    cur.set(cur.get.tail: List[() ->{xs*} Unit])

  usingFile: f =>
    cur.set:
      (() => f.write(): () ->{f*} Unit) :: Nil // error since {f*} !<: {xs*}
```

More examples. This works:
```scala
def cons(x: Proc, xs: List[Proc]): List[() ->{x, xs*} Unit] =
  List.cons[() ->{x, xs*} Unit](x, xs)
```
And this works as well
```scala
def addOneProc(xs: List[Proc]): List[Proc] =
  def x: Proc = () => write("hello")
  val result: List[() ->{x, xs*} Unit] = x :: xs
  result // OK, we can widen () ->{x, xs*} Unit to cap here.
```
This doesn't work:
```scala
def cons(x: Proc, xs: Set[Proc]) =
  Set.include[Proc](x, xs) // error: can't instantiate type parameter to Proc
```
But this works:
```scala
def cons(x: Proc, xs: Set[Proc]): Set[() ->{x,xs*} Unit] =
  Set.include[() ->{x,xs*} Unit](x, xs) // ok
```
Say we have `a: () ->{io} Unit` and `as: List[() ->{io} Unit]`. Then `cons(a, as)`
is of type `() ->{a, as*} Unit`, which is a subtype of `() ->{io} Unit`. This follows from
`{a} <: {io}` by rule (Var) and `{as*} <: dcs(as) = {io}` by the subcapturing rules for
reach capabilities.

This also works:
```scala
def compose1[A, B, C](f: A => B, g: B => C): A ->{f, g} C =
  z => g(f(z))
```
And this works as well:
```scala
def compose2[A, B, C](f: A => B, g: B => C): A => C =
  z => g(f(z))
```
Even this should work:
```scala
def mapCompose[A](ps: List[(A => A, A => A)]): List[A ->{ps*} A] =
  ps.map(compose1)
    // ps: List[(A ->{ps*} A, A ->{ps*} A)]
    // Hence compose1's parameters are both of type A ->{ps*} A
    // Hence its result type is A ->{ps*} A
    // So map's type parameter is A ->{ps*} A
    // Expanded typing:
    //   (ps: List[(A ->{ps*} A, A ->{ps*} A)])
    //     .map[A ->{ps*} A]: (f: A ->{ps*} A, g: A ->{ps*} A) =>
    //       compose1[A ->{ps*} A, A ->{ps*} A, A ->{ps*} A](f, g)
    //         : A -> {f, g} A
    //  The closure is widened to the non-dependent function type
    //    (f: A ->{ps*} A, g: A ->{ps*} A) -> A ->{ps*} A
```
But it does not work with `compose2`, since the type variable of `map` cannot be instantiated to `A => A`.

Syntax Considerations:

 - `x*` is short and has the right connotations. For the spread operator, `xs*` means
   _everything contained in x_. Likewise `x*` in capture sets would mean all capabilities
   reachable through `x`.
 - But then we have capabilities that are not values, undermining the OCap model a bit.
   On the other hand, even if we make `x*` values then these would have to be erased in any case.

Work items:
===========

 - Implement `x*` references.
    - internal representation: maybe have a synthetic private member `*` of
      `Any` to which `x*` maps, i.e. `x*` is `x.*`. Advantage: maps like substitutions
      and asSeenFrom work out of the box.
    - subcapturing: `x <:< x* <: dcs(x)`.
    - Narrowing code: in `adaptBoxed` where `x.type` gets widened to `T^{x}`, also
      do the covariant `cap` to `x*` replacement. Similarly in `fourthTry` of `TypeComparer`.
 - Drop local roots
 - Make all type paraneters sealed

