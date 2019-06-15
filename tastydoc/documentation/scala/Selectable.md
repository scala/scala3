scala
# trait Selectable

<pre><code class="language-scala" >trait Selectable extends Any</pre></code>
## Constructors:
<pre><code class="language-scala" >Selectable()</pre></code>

## Concrete Value Members:
### applyDynamic
<pre><code class="language-scala" >def applyDynamic(name: String, paramClasses: Seq[<a href="./reflect/ClassTag.md">ClassTag</a>[Nothing <: Any]])(args: Seq[Any]): Any</pre></code>

### selectDynamic
<pre><code class="language-scala" >def selectDynamic(name: String): Any</pre></code>

