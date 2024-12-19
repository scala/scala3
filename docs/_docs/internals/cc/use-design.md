
Possible design:

 1. Have @use annotation on type parameters and value parameters of regular methods
   (not anonymous functions).
 2. In markFree, keep track whether a capture set variable or reach capability
   is used directly in the method where it is defined, or in a nested context
   (either unbound nested closure or unbound anonymous class).
 3. Disallow charging a reach capability `xs*` to the environment of the method where
   `xs` is a parameter unless `xs` is declared `@use`.
 4. Analogously, disallow charging a capture set variable `C^` to the environment of the method where `C^` is a parameter unless `C^` is declared `@use`.
 5. When passing an argument to a `@use`d term parameter, charge the `dcs` of the argument type to the environments via markFree.
 6. When instantiating a `@use`d type parameter, charge the capture set of the argument
   to the environments via markFree.

It follows that we cannot refer to methods with @use term parameters as values. Indeed,
their eta expansion would produce an anonymous function that includes a reach capability of
its parameter in its use set, violating (3).

Example:

```scala
  def runOps(@use ops: List[() => Unit]): Unit = ops.foreach(_())
```
Then `runOps` expands to
```scala
(xs: List[() => Unit]) => runOps(xs)
```
Note that `xs` does not carry a `@use` since this is disallowed by (1) for anonymous functions. By (5), we charge the deep capture set of `xs`, which is `xs*` to the environment. By (3), this is actually disallowed.

Now, if we express this with explicit capture set parameters we get:
```scala
  def runOpsPoly[@use C^](ops: List[() ->{C^} Unit]): Unit = ops.foreach[C^](_())
```
Then `runOpsPoly` expands to `runOpsPoly[cs]` for some inferred capture set `cs`. And this expands to:
```scala
(xs: List[() ->{cs} Unit]) => runOpsPoly[cs](xs)
```
Since `cs` is passed to the `@use` parameter of `runOpsPoly` it is charged
to the environment of the function body, so the type of the previous expression is
```scala
List[() ->{cs} Unit]) ->{cs} Unit
```

We can also use explicit capture set parameters to eta expand the first `runOps` manually:

```scala
[C^] => (xs: List[() ->{C^} Unit]) => runOps(xs)
  : [C^] -> List[() ->{C^} Unit] ->[C^] Unit
```
Except that this currently runs afoul of the implementation restriction that polymorphic functions cannot wrap capturing functions. But that's a restriction we need to lift anyway.

## `@use` inference

 - `@use` is implied for a term parameter `x` of a method if `x`'s type contains a boxed cap and `x` or `x*` is not referred to in the result type of the method.

 - `@use` is implied for a capture set parameter `C` of a method if `C` is not referred to in the result type of the method.

If `@use` is implied, one can override to no use by giving an explicit use annotation
`@use(false)` instead. Example:
```scala
  def f(@use(false) xs: List[() => Unit]): Int = xs.length
```

This works since `@use` is defined like this:
```scala
class use(cond: Boolean = true) extends StaticAnnotation
```



