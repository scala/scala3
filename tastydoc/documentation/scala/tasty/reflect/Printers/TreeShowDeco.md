scala.tasty.reflect.Printers
# class TreeShowDeco

<pre><code class="language-scala" >class TreeShowDeco</pre></code>
Adds `show` as an extension method of a `Tree`

## Constructors:
<pre><code class="language-scala" >TreeShowDeco(tree: Tree)</pre></code>

## Concrete Value Members:
### show
<pre><code class="language-scala" >def show(implicit ctx: Context): String</pre></code>
Shows the tree as fully typed source code.
Will print Ansi colors if ctx.printColors is enabled.

### showExtractors
<pre><code class="language-scala" >def showExtractors(implicit ctx: Context): String</pre></code>
Shows the tree as extractors

