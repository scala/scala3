scala.implicits
# class Not

## Companion object <a href="./Not$.md">Not</a>

<pre><code class="language-scala" >final class Not[T]</pre></code>
A special class used to implement negation in implicit search.
Consider the problem of using implicit `i1` for a query type `D` if an implicit
for some other class `C` is available, and using an implicit `i2` if no implicit
value of type `C` is available. If we do not want to prioritize `i1` and `i2` by
putting them in different traits we can instead define the following:
   implicit def i1: D(implicit ev: C) = ...
   implicit def i2: D(implicit ev: Not[C]) = ...
`Not` is treated specially in implicit search, similar to the way logical negation
is treated in Prolog: The implicit search for `Not[C]` succeeds if and only if the implicit
search for `C` fails.
In Scala 2 this form of negation can be simulated by setting up a conditional
ambiguous implicit and an unconditional fallback, the way it is done with the
`default`, `amb1` and `amb2` methods below. Due to the way these two methods are
defined, `Not` is also usable from Scala 2.
In Dotty, ambiguity is a global error, and therefore cannot be used to implement negation.
Instead, `Not` is treated natively in implicit search.

