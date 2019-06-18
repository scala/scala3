scala.tasty.reflect.TreeOps
# class TermAPI

<pre><code class="language-scala" >class TermAPI</pre></code>
## Constructors:
<pre><code class="language-scala" >TermAPI(self: Term)</pre></code>

## Concrete Value Members:
### appliedTo
<pre><code class="language-scala" >def appliedTo(arg: Term, args: Seq[Term])(implicit ctx: Context): Term</pre></code>
An apply node with given arguments: `tree(arg, args0, ..., argsN)`

### appliedTo
<pre><code class="language-scala" >def appliedTo(arg: Term)(implicit ctx: Context): Term</pre></code>
A unary apply node with given argument: `tree(arg)`

### appliedToArgs
<pre><code class="language-scala" >def appliedToArgs(args: List[Term])(implicit ctx: Context): Apply</pre></code>
An apply node with given argument list `tree(args(0), ..., args(args.length - 1))`

### appliedToArgss
<pre><code class="language-scala" >def appliedToArgss(argss: List[List[Term]])(implicit ctx: Context): Term</pre></code>
The current tree applied to given argument lists:
`tree (argss(0)) ... (argss(argss.length -1))`

### appliedToNone
<pre><code class="language-scala" >def appliedToNone(implicit ctx: Context): Apply</pre></code>
The current tree applied to (): `tree()`

### appliedToType
<pre><code class="language-scala" >def appliedToType(targ: Type)(implicit ctx: Context): Term</pre></code>
The current tree applied to given type argument: `tree[targ]`

### appliedToTypeTrees
<pre><code class="language-scala" >def appliedToTypeTrees(targs: List[TypeTree])(implicit ctx: Context): Term</pre></code>
The current tree applied to given type argument list: `tree[targs(0), ..., targs(targs.length - 1)]`

### appliedToTypes
<pre><code class="language-scala" >def appliedToTypes(targs: List[Type])(implicit ctx: Context): Term</pre></code>
The current tree applied to given type arguments: `tree[targ0, ..., targN]`

### pos
<pre><code class="language-scala" >def pos(implicit ctx: Context): Position</pre></code>

### select
<pre><code class="language-scala" >def select(sym: Symbol)(implicit ctx: Context): Select</pre></code>
A select node that selects the given symbol.

### tpe
<pre><code class="language-scala" >def tpe(implicit ctx: Context): Type</pre></code>

### underlying
<pre><code class="language-scala" >def underlying(implicit ctx: Context): Term</pre></code>

### underlyingArgument
<pre><code class="language-scala" >def underlyingArgument(implicit ctx: Context): Term</pre></code>

