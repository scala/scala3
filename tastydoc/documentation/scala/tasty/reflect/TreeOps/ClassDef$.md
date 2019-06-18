scala.tasty.reflect.TreeOps
# object ClassDef

<pre><code class="language-scala" >final object ClassDef extends Serializable</pre></code>
## Concrete Value Members:
### copy
<pre><code class="language-scala" >def copy(original: ClassDef)(name: String, constr: DefDef, parents: List[Tree], derived: List[TypeTree], selfOpt: Option[ValDef], body: List[Statement])(implicit ctx: Context): ClassDef</pre></code>

### unapply
<pre><code class="language-scala" >def unapply(tree: Tree)(implicit ctx: Context): Option[(String, DefDef, List[Tree], List[TypeTree], Option[ValDef], List[Statement])]</pre></code>

