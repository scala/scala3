scala.quoted.matching
# object Const

<pre><code class="language-scala" >final object Const extends Serializable</pre></code>
Matches expressions containing literal constant values and extracts the value.
It may match expressions of type Boolean, Byte, Short, Int, Long,
Float, Double, Char, String, ClassTag, scala.Symbol, Null and Unit.
Usage:
```
(x: Expr[B]) match {
  case Const(value: B) => ...
}
```
``````

## Concrete Value Members:
### unapply
<pre><code class="language-scala" >def unapply[T](expr: <a href="../Expr.md">Expr</a>[T])(implicit reflect: <a href="../../tasty/Reflection.md">Reflection</a>): Option[T]</pre></code>

