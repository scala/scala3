scala.tasty.reflect.ConstantOps.Constant$
# object ClassTag

<pre><code class="language-scala" >final object ClassTag extends Serializable</pre></code>
Module of ClassTag literals

## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply[T](implicit x: <a href="../../../../reflect/ClassTag.md">ClassTag</a>[T]): Constant</pre></code>
scala.reflect.ClassTag literal

### unapply
<pre><code class="language-scala" >def unapply(constant: Constant): Option[Type]</pre></code>
Extractor for ClassTag literals

