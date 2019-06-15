scala.tasty.reflect.TreeOps
# object Select

<pre><code class="language-scala" >final object Select extends Serializable</pre></code>
Scala term selection

## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(qualifier: Term, symbol: Symbol)(ctx: Context): Select</pre></code>
Select a term member by symbol

### copy
<pre><code class="language-scala" >def copy(original: Tree)(qualifier: Term, name: String)(ctx: Context): Select</pre></code>

### overloaded
<pre><code class="language-scala" >def overloaded(qualifier: Term, name: String, targs: List[Type], args: List[Term])(ctx: Context): Apply</pre></code>
Call an overloaded method with the given type and term parameters

### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(ctx: Context): Option[(Term, String)]</pre></code>
Matches `<qualifier: Term>.<name: String>`

### unique
<pre><code class="language-scala" >def unique(qualifier: Term, name: String)(ctx: Context): Select</pre></code>
Select a field or a non-overloaded method by name

***Note*** The method will produce an assertion error if the selected
      method is overloaded. The method `overloaded` should be used
      in that case.

