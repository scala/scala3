scala
# class Conversion

<pre><code class="language-scala" >abstract class Conversion[T, U] extends (<a href="./Conversion.md#T">T</a>) => <a href="./Conversion.md#U">U</a></pre></code>
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

## Annotations:
@FunctionalInterface 
## Constructors:
<pre><code class="language-scala" >Conversion()</pre></code>

## Concrete Value Members:
### andThen
<pre><code class="language-scala" >@unspecialized def andThen[A](g: (R) => A): (T1) => A</pre></code>

### apply
<pre><code class="language-scala" >def apply(v1: T1): R</pre></code>

### compose
<pre><code class="language-scala" >@unspecialized def compose[A](g: (A) => T1): (A) => R</pre></code>

### toString
<pre><code class="language-scala" >override def toString(): String</pre></code>

