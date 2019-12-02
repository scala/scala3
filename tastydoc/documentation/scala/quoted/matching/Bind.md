scala.quoted.matching
# class Bind

## Companion object <a href="./Bind$.md">Bind</a>

<pre><code class="language-scala" >class Bind[T <: scala.AnyKind]</pre></code>
Bind of an Expr[T] used to know if some Expr[T] is a reference to the binding

***name*** string name of this binding

***id*** unique id used for equality

## Constructors:
<pre><code class="language-scala" >Bind(name: String, id: Object)</pre></code>

## Concrete Value Members:
### equals
<pre><code class="language-scala" >override def equals(obj: Any): Boolean</pre></code>

### hashCode
<pre><code class="language-scala" >override def hashCode(): Int</pre></code>

### id
<pre><code class="language-scala" >private[Bind] val id: Object</pre></code>

### name
<pre><code class="language-scala" >val name: String</pre></code>

