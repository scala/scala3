A capture checking variant
==========================

 - We use a stricter form of the TOPLAS model where
    - we can't unbox and box from and into S^{cap} (as in TOPLAS), and
    - we don't allow subtyping to cap under box. I.e.
      ```
      Box S1 ^ C1 <: Box S2 ^ C2
      if S1 <: S2, C1 <: C2, C2 != {cap}
      ```
   The aim is to have a system where we detect all leaks on formation
   and not some just on access. In the TOPLAS version, we cannot box {cap}
   but we can box with some capability {x} and then widen under the box
   into {cap}. This was not a problem because we cannot unbox cap so the
   data could not be accessed. But now we want to be stricter and already
   prevent the formation. This strictness is necessary to ensure soundness
   of the `cap -> x*` conversion described below.

 - To compensate, and allow capture polymorphism in e.g. function arguments, we allow
   some subcapture slack to cap under box when comparing the types of actual and expected types
   after rechecking. This has to be done selectively, so that the following are still guaranteed:

    - No widening of function results.
      That way, library-defined encapsulators like `usingFile` still prevent leaks
      on formation.
    - No widening in assignments to vars.
      That way, we cannot assign local capabilities to global vars.
    - No widening in results of trys.
      That way, local throws capabilities cannot escape.

 - The way to achieve this is to tweak the expected type. Interesting
   expected types are created by

    1. function parameter types for their arguments,
    2. declared types of vals or vars for their right hand sides,
    3. declared result types of defs for their right hand sides,
    4. declared types of vars for the right hand sides of assignments to them,
    5. declared types of seq literals for the elements in that seq literal,

  In cases (1) and (2) above we map all covariant occurrences of cap
  in the original expected type to a wildcard (i.e. a fluid capture set). This still treats
  `try` blocks correctly, since even if the expected type of a try block contains wildcards,
  we still need to eliminate capabilities defined in the try through avoidance, and that
  will not be possible since we cannot widen to cap via subtyping.

 - Technicalities for type comparers and type maps:

    1. Subcapturing: We need to thread through the subset propagation logic whether
       the elements to add to a capture set come from a boxed set. Maybe it's best
       for this if the isBoxed flag was moved from CapturingTypes to CaptureSets?
       Or, alternativelty, pass box status to subCaptures as an additional parameter,
       but then we also have to deal with uses of subCapturing in, say,
       set union or intersection. The knowledge that an element comes from a
       boxed set has to be propagated through maps. I.e. if r comes from a boxed
       set, we also assume f(r) comes from a boxed set. Then, the `x.subsumes(y)`
       test needs to know whether `y` comes from a boxed set. All this looks
       rather complicated.

    2. Lubs of capture sets can now contain at the same time `cap` and other
       references.

    3. Avoidance maps can have undefined results. We need to tweak the part
       of AvoidMap that widens from a TermRef `ref` to `ref.info`, so that
       this is forbidden if the widening appears in a boxed capture set.
       This could be achieved by disallowing the root capability in a capture
       set that arises from mapping a boxed capture set through avoidance, using
       a handler that issues an appropriate error message.

 - As in the TOPLAS paper, mutable variables and results of try are implicitly boxed.

 - For any immutable variable `x`, introduce a capability `x*` which stands for
   "all capabilities reachable through `x`". We have `{x} <: {x*} <: {cap}`.

 - We modify the VAR rule as follows:

        x: T in E
        -----------
        E |- x: T'

   where T' is T with (1) the toplevel capture set replaced by `{x}` and
   (2) all covariant occurrences of cap replaced by `x*`. (1) is standard,
   whereas (2) is new.

- Why is this sound? Covariant occurrences of cap must represent capabilities
  that are reachable from `x`, so they are included in the meaning of `{x*}`.


Examples:

```scala
class Ref[T](init: T):
  private var x: T
  def get: T = x
  def set(y: T) = { x = y }
```

The following example does not work.
```scala
def runAll(xs: List[Proc]): Unit =
  var cur: List[() => Unit] = xs
  while cur.nonEmpty do
    val next: () => Unit = cur.head // error on unbox
    next()
    cur = cur.tail

  usingFile: f =>
    cur = ((() => f.write()): (() ->{f*} Unit)) :: Nil // error since we cannot widen {f*} to {cap} under box
```
Same with refs:
```scala
def runAll(xs: List[Proc]): Unit =
  val cur = Ref[List[Proc]](xs: List[() ->{xs*} Unit]) // error on box
  while cur.get.nonEmpty do
    val next: () => Unit = cur.get.head // error on unbox
    next()
    cur.set(cur.get.tail: List[Proc])

  usingFile: f =>
    cur.set:
      (() => f.write(): () ->{f*} Unit) :: Nil // error since we cannot widen {f*} to {cap} under box
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
    cur = (() => f.write(): () ->{f*} Unit) :: Nil // error since {f*} !<: {xs*}
```

The following variant of the while loop is again invalid:
```scala
def runAll(xs: List[Proc]): Unit =
  var cur: List[Proc] = xs // error, can't widen under box, xs: List[() ->{xs*} Unit]
  while cur.nonEmpty do
    val next: () ->{cur*} Unit = cur.head: // error: cur* not wf since cur is not stable
    next()
    cur = cur.tail
```
More examples. This works:
```scala
def cons(x: Proc, xs: List[Proc]): List[() ->{x, xs*} Unit] =
  List.cons[() ->{x, xs*} Unit](x, xs)
```
But this doesn't:
```scala
def addOneProc(xs: List[Proc]) =
  def x: Proc = () => write("hello")
  val result: List[() ->{x, xs*} Unit] = x :: xs
  result // error: can't widen to cap under box in function result
```
And this doesn't either, since `Set` is invariant:
```scala
def cons(x: Proc, xs: Set[Proc]) =
  Set.include[Proc](x, xs) // error: can't instantiate type parameter to Proc
```
But this works:
```scala
def cons(x: Proc, xs: Set[Proc]): Set[() ->{x,xs*} Unit] =
  Set.include[() ->{x,xs*} Unit](x, xs) // ok
```

This also works:
```scala
def compose1[A, B, C](f: A => B, g: B => C): A ->{f, g} C =
  z => g(f(z))
```
We can also use a widened result type for compose:
```scala
def compose2[A, B, C](f: A => B, g: B => C): A => C =
  z => g(f(z))
```
Even this should workL
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

But this would not work with `compose2` instead of `compose1`.

Work items:
===========

 - Implement x* references.
    - internal representation: maybe have a synthetic private member `reach` of
      `Any` to which `x*` maps.
    - subcapturing: `x <:< x*`.
    - Narrowing code: in `adaptBoxed` where `x.type` gets widened to `T^{x}`, also
      do the covariant `cap` to `x*` replacement.
 - Drop local roots
 - Implement restricted subtyping
 - Implement adaptation that widens under box
 - Drop sealed scheme

def compose(f: A => B, g: B => C): A ->{f, g} C
