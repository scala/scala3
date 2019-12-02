scala.tasty.reflect.TypeOrBoundsOps
# class TypeAPI

<pre><code class="language-scala" >class TypeAPI</pre></code>
## Constructors:
<pre><code class="language-scala" >TypeAPI(self: Type)</pre></code>

## Concrete Value Members:
### <:<
<pre><code class="language-scala" >def <:<(that: Type)(implicit ctx: Context): Boolean</pre></code>

### =:=
<pre><code class="language-scala" >def =:=(that: Type)(implicit ctx: Context): Boolean</pre></code>

### classSymbol
<pre><code class="language-scala" >def classSymbol(implicit ctx: Context): Option[ClassDefSymbol]</pre></code>

### dealias
<pre><code class="language-scala" >def dealias(implicit ctx: Context): Type</pre></code>
Follow aliases and dereferences LazyRefs, annotated types and instantiated
TypeVars until type is no longer alias type, annotated type, LazyRef,
or instantiated type variable.

### derivesFrom
<pre><code class="language-scala" >def derivesFrom(cls: ClassDefSymbol)(implicit ctx: Context): Boolean</pre></code>
Is this type an instance of a non-bottom subclass of the given class `cls`?

### isDependentFunctionType
<pre><code class="language-scala" >def isDependentFunctionType(implicit ctx: Context): Boolean</pre></code>
Is this type a dependent function type?

***see*** `isFunctionType`

### isErasedFunctionType
<pre><code class="language-scala" >def isErasedFunctionType(implicit ctx: Context): Boolean</pre></code>
Is this type an erased function type?

***see*** `isFunctionType`

### isFunctionType
<pre><code class="language-scala" >def isFunctionType(implicit ctx: Context): Boolean</pre></code>
Is this type a function type?

***return*** true if the dealised type of `self` without refinement is `FunctionN[T1, T2, ..., Tn]`

***Note*** The function
* returns true for `given Int => Int` and `erased Int => Int`
* returns false for `List[Int]`, despite that `List[Int] <:< Int => Int`.


### isImplicitFunctionType
<pre><code class="language-scala" >def isImplicitFunctionType(implicit ctx: Context): Boolean</pre></code>
Is this type an implicit function type?

***see*** `isFunctionType`

### isSingleton
<pre><code class="language-scala" >def isSingleton(implicit ctx: Context): Boolean</pre></code>

### memberType
<pre><code class="language-scala" >def memberType(member: Symbol)(implicit ctx: Context): Type</pre></code>

### typeSymbol
<pre><code class="language-scala" >def typeSymbol(implicit ctx: Context): Symbol</pre></code>

### widen
<pre><code class="language-scala" >def widen(implicit ctx: Context): Type</pre></code>

