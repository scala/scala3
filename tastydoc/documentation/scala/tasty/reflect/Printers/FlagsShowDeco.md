scala.tasty.reflect.Printers
# class FlagsShowDeco

<pre><code class="language-scala" >class FlagsShowDeco</pre></code>
Adds `show` as an extension method of a `Flags`

## Constructors:
<pre><code class="language-scala" >FlagsShowDeco(flags: Flags)</pre></code>

## Concrete Value Members:
### show
<pre><code class="language-scala" >def show(implicit ctx: Context): String</pre></code>
Shows the tree as fully typed source code.
Will print Ansi colors if ctx.printColors is enabled.

### showExtractors
<pre><code class="language-scala" >def showExtractors(implicit ctx: Context): String</pre></code>
Shows the tree as extractors

