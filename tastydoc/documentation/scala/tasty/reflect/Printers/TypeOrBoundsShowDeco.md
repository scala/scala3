scala.tasty.reflect.Printers
# class TypeOrBoundsShowDeco

<pre><code class="language-scala" >class TypeOrBoundsShowDeco</pre></code>
Adds `show` as an extension method of a `TypeOrBounds`

## Constructors:
<pre><code class="language-scala" >TypeOrBoundsShowDeco(tpe: TypeOrBounds)</pre></code>

## Concrete Value Members:
### show
<pre><code class="language-scala" >def show(implicit ctx: Context): String</pre></code>
Shows the tree as fully typed source code.
Will print Ansi colors if ctx.printColors is enabled.

### showExtractors
<pre><code class="language-scala" >def showExtractors(implicit ctx: Context): String</pre></code>
Shows the tree as extractors

