scala.tasty.reflect.TreeOps
# object ImpliedMatch

<pre><code class="language-scala" >final object ImpliedMatch extends Serializable</pre></code>
Scala implicit `match` term

## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(cases: List[CaseDef])(ctx: Context): ImpliedMatch</pre></code>
Creates a pattern match `delegate match { <cases: List[CaseDef]> }`

### copy
<pre><code class="language-scala" >def copy(original: Tree)(cases: List[CaseDef])(ctx: Context): ImpliedMatch</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(ctx: Context): Option[List[CaseDef]]</pre></code>
Matches a pattern match `delegate match { <cases: List[CaseDef]> }`

