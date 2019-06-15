# Package matching
## Members:
<pre><code class="language-scala" >final object <a href="./ExprSeq$.md">ExprSeq</a></pre></code>
Literal sequence of expressions

<pre><code class="language-scala" >final val ExprSeq: ExprSeq</pre></code>
Literal sequence of expressions


<pre><code class="language-scala" >final object <a href="./Bind$.md">Bind</a></pre></code>
<pre><code class="language-scala" >final val Bind: Bind</pre></code>

<pre><code class="language-scala" >class <a href="./Bind.md">Bind</a></pre></code>
Bind of an Expr[T] used to know if some Expr[T] is a reference to the binding

***name*** string name of this binding

***id*** unique id used for equality

<pre><code class="language-scala" >final object <a href="./Const$.md">Const</a></pre></code>
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

<pre><code class="language-scala" >final val Const: Const</pre></code>
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


<pre><code class="language-scala" >final object <a href="./ConstSeq$.md">ConstSeq</a></pre></code>
Literal sequence of literal constant value expressions

<pre><code class="language-scala" >final val ConstSeq: ConstSeq</pre></code>
Literal sequence of literal constant value expressions


