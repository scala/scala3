scala.reflect
# class Selectable

## Companion object <a href="./Selectable$.md">Selectable</a>

<pre><code class="language-scala" >final class Selectable extends AnyVal with Selectable</pre></code>
## Constructors:
<pre><code class="language-scala" >Selectable(receiver: Any)</pre></code>

## Concrete Value Members:
### applyDynamic
<pre><code class="language-scala" >override def applyDynamic(name: String, paramTypes: Seq[<a href="./ClassTag.md">ClassTag</a>[Nothing <: Any]])(args: Seq[Any]): Any</pre></code>

### selectDynamic
<pre><code class="language-scala" >def selectDynamic(name: String): Any</pre></code>

### receiver
<pre><code class="language-scala" >val receiver: Any</pre></code>

