# Package scala
## Members:
<pre><code class="language-scala" >trait Selectable</pre></code>
<pre><code class="language-scala" >final object *:</pre></code>
<pre><code class="language-scala" >final val *:: *:</pre></code>

<pre><code class="language-scala" >@showAsInfix sealed abstract class *:</pre></code>
<pre><code class="language-scala" >sealed trait NonEmptyTuple</pre></code>
Tuple of arbitrary non-zero arity

<pre><code class="language-scala" >class forceInline</pre></code>
An annotation on methods that is equivalent to Dotty `inline` modifier,
except that it does not imply `erased`.
The annotation should be used instead of the `inline` modifier in code
that needs to cross compile between Scala 2 and Dotty.
Note that Scala 2 ignores the `@forceInLine` annotation, and one must use
both the `@inline` and `@forceInline` annotation to inline across the
two compilers. E.g.
```scala```

<pre><code class="language-scala" >@FunctionalInterface abstract class Conversion</pre></code>
A class for implicit values that can serve as implicit conversions
The implicit resolution algorithm will act as if there existed
the additional implicit definition:
  def $implicitConversion[T, U](x: T)(c: Conversion[T, U]): U = c(x)
However, the presence of this definition would slow down implicit search since
its outermost type matches any pair of types. Therefore, implicit search
contains a special case in `Implicits#discardForView` which emulates the
conversion in a more efficient way.
Note that this is a SAM class - function literals are automatically converted
to the `Conversion` values.
Also note that in bootstrapped dotty, `Predef.<:<` should inherit from
`Conversion`. This would cut the number of special cases in `discardForView`
from two to one.

<pre><code class="language-scala" >@implicitNotFound sealed trait TupledFunction</pre></code>
Type class relating a `FunctionN[..., R]` with an equivalent tupled function `Function1[TupleN[...], R]`

***F*** a function type

***G*** a tupled function type (function of arity 1 receiving a tuple as argument)

<pre><code class="language-scala" >trait FunctionXXL</pre></code>
A function with all parameters grouped in an array.

<pre><code class="language-scala" >final object TupleXXL</pre></code>
<pre><code class="language-scala" >final val TupleXXL: TupleXXL</pre></code>

<pre><code class="language-scala" >final class TupleXXL</pre></code>
<pre><code class="language-scala" >final object Eql</pre></code>
Companion object containing a few universally known `Eql` instances.
Eql instances involving primitive types or the Null type are handled directly in
the compiler (see Implicits.synthesizedEq), so they are not included here.

<pre><code class="language-scala" >final val Eql: Eql</pre></code>
Companion object containing a few universally known `Eql` instances.
Eql instances involving primitive types or the Null type are handled directly in
the compiler (see Implicits.synthesizedEq), so they are not included here.


<pre><code class="language-scala" >@implicitNotFound sealed trait Eql</pre></code>
A marker trait indicating that values of type `L` can be compared to values of type `R`.

<pre><code class="language-scala" >trait Product0</pre></code>
<pre><code class="language-scala" >final object Product0</pre></code>
A class for Product0 which was missing from the scala distribution.

<pre><code class="language-scala" >final val Product0: Product0</pre></code>
A class for Product0 which was missing from the scala distribution.


<pre><code class="language-scala" >trait PolyFunction</pre></code>
Marker trait for polymorphic function types.
This is the only trait that can be refined with a polymorphic method,
as long as that method is called `apply`, e.g.:
    PolyFunction { def apply[T_1, ..., T_M](x_1: P_1, ..., x_N: P_N): R }
This type will be erased to FunctionN.

<pre><code class="language-scala" >final object deriving</pre></code>
<pre><code class="language-scala" >final val deriving: deriving</pre></code>

<pre><code class="language-scala" >final object Tuple</pre></code>
<pre><code class="language-scala" >final val Tuple: Tuple</pre></code>

<pre><code class="language-scala" >sealed trait Tuple</pre></code>
Tuple of arbitrary arity

<pre><code class="language-scala" >final object IArray$package</pre></code>
<pre><code class="language-scala" >final val IArray$package: IArray$package</pre></code>

<pre><code class="language-scala" >trait Enum</pre></code>
A base trait of all enum classes

<pre><code class="language-scala" >@implicitNotFound final object ValueOf</pre></code>
`ValueOf[T]` provides the unique value of the type `T` where `T` is a type which has a
single inhabitant. Eligible types are singleton types of the form `stablePath.type`,
Unit and singleton types corresponding to value literals.
Instances of `ValueOf[T]` are provided implicitly for all eligible types. Typically
an instance would be required where a runtime value corresponding to a type level
computation is needed.
For example, we might define a type `Residue[M <: Int]` corresponding to the group of
integers modulo `M`. We could then mandate that residues can be summed only when they
are parameterized by the same modulus,
```scala
case class Residue[M <: Int](n: Int) extends AnyVal {
 def +(rhs: Residue[M])(implicit m: ValueOf[M]): Residue[M] =
   Residue((this.n + rhs.n) % valueOf[M])
}

val fiveModTen = Residue[10](5)
val nineModTen = Residue[10](9)

fiveModTen + nineModTen    // OK == Residue[10](4)

val fourModEleven = Residue[11](4)

fiveModTen + fourModEleven // compiler error: type mismatch;
                          //   found   : Residue[11]
                          //   required: Residue[10]
```
Notice that here the modulus is encoded in the type of the values and so does not
incur any additional per-value storage cost. When a runtime value of the modulus
is required in the implementation of `+` it is provided at the call site via the
implicit argument `m` of type `ValueOf[M]`.

<pre><code class="language-scala" >@implicitNotFound final val ValueOf: ValueOf</pre></code>
`ValueOf[T]` provides the unique value of the type `T` where `T` is a type which has a
single inhabitant. Eligible types are singleton types of the form `stablePath.type`,
Unit and singleton types corresponding to value literals.
Instances of `ValueOf[T]` are provided implicitly for all eligible types. Typically
an instance would be required where a runtime value corresponding to a type level
computation is needed.
For example, we might define a type `Residue[M <: Int]` corresponding to the group of
integers modulo `M`. We could then mandate that residues can be summed only when they
are parameterized by the same modulus,
```scala
case class Residue[M <: Int](n: Int) extends AnyVal {
 def +(rhs: Residue[M])(implicit m: ValueOf[M]): Residue[M] =
   Residue((this.n + rhs.n) % valueOf[M])
}

val fiveModTen = Residue[10](5)
val nineModTen = Residue[10](9)

fiveModTen + nineModTen    // OK == Residue[10](4)

val fourModEleven = Residue[11](4)

fiveModTen + fourModEleven // compiler error: type mismatch;
                          //   found   : Residue[11]
                          //   required: Residue[10]
```
Notice that here the modulus is encoded in the type of the values and so does not
incur any additional per-value storage cost. When a runtime value of the modulus
is required in the implementation of `+` it is provided at the call site via the
implicit argument `m` of type `ValueOf[M]`.


<pre><code class="language-scala" >@implicitNotFound final class ValueOf</pre></code>
`ValueOf[T]` provides the unique value of the type `T` where `T` is a type which has a
single inhabitant. Eligible types are singleton types of the form `stablePath.type`,
Unit and singleton types corresponding to value literals.
Instances of `ValueOf[T]` are provided implicitly for all eligible types. Typically
an instance would be required where a runtime value corresponding to a type level
computation is needed.
For example, we might define a type `Residue[M <: Int]` corresponding to the group of
integers modulo `M`. We could then mandate that residues can be summed only when they
are parameterized by the same modulus,
```scala
case class Residue[M <: Int](n: Int) extends AnyVal {
 def +(rhs: Residue[M])(implicit m: ValueOf[M]): Residue[M] =
   Residue((this.n + rhs.n) % valueOf[M])
}

val fiveModTen = Residue[10](5)
val nineModTen = Residue[10](9)

fiveModTen + nineModTen    // OK == Residue[10](4)

val fourModEleven = Residue[11](4)

fiveModTen + fourModEleven // compiler error: type mismatch;
                          //   found   : Residue[11]
                          //   required: Residue[10]
```
Notice that here the modulus is encoded in the type of the values and so does not
incur any additional per-value storage cost. When a runtime value of the modulus
is required in the implementation of `+` it is provided at the call site via the
implicit argument `m` of type `ValueOf[M]`.

<pre><code class="language-scala" >package <a href="./compiletime/compiletime.md">compiletime</a></pre></code><pre><code class="language-scala" >package <a href="./internal/internal.md">internal</a></pre></code><pre><code class="language-scala" >package <a href="./quoted/quoted.md">quoted</a></pre></code>