scala.tasty.reflect.TreeOps
# object NamedArg

<pre><code class="language-scala" >final object NamedArg extends Serializable</pre></code>
Scala named argument `x = y` in argument position

## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(name: String, arg: Term)(implicit ctx: Context): NamedArg</pre></code>
Create a named argument `<name: String> = <value: Term>`

### copy
<pre><code class="language-scala" >def copy(original: NamedArg)(name: String, arg: Term)(implicit ctx: Context): NamedArg</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(implicit ctx: Context): Option[(String, Term)]</pre></code>
Matches a named argument `<name: String> = <value: Term>`

