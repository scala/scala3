scala.tasty.reflect.CommentOps
# class CommentAPI

<pre><code class="language-scala" >class CommentAPI</pre></code>
## Constructors:
<pre><code class="language-scala" >CommentAPI(self: Comment)</pre></code>

## Concrete Value Members:
### expanded
<pre><code class="language-scala" >def expanded: Option[String]</pre></code>
Expanded comment string, if any

### raw
<pre><code class="language-scala" >def raw: String</pre></code>
Raw comment string

### usecases
<pre><code class="language-scala" >def usecases: List[(String, Option[DefDef])]</pre></code>
List of usecases and their corresponding trees, if any

