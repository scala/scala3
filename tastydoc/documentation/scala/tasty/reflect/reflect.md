# Package reflect
## Members:
<pre><code class="language-scala" >trait <a href="./QuotedOps.md">QuotedOps</a></pre></code>
Extension methods on scala.quoted.{Expr|Type} to convert to scala.tasty.Tasty objects

<pre><code class="language-scala" >trait <a href="./ImportSelectorOps.md">ImportSelectorOps</a></pre></code>
<pre><code class="language-scala" >trait <a href="./TreeUtils.md">TreeUtils</a></pre></code>
Tasty reflect case definition

<pre><code class="language-scala" >trait <a href="./Kernel.md">Kernel</a></pre></code>
Tasty reflect abstract types
```none```
+- Tree -+- PackageClause
         +- Import
         +- Statement -+- Definition --+- PackageDef
         |             |               +- ClassDef
         |             |               +- TypeDef
         |             |               +- DefDef
         |             |               +- ValDef
         |             |
         |             +- Term --------+- Ref -+- Ident
         |                             |       +- Select
         |                             |
         |                             +- Literal
         |                             +- This
         |                             +- New
         |                             +- NamedArg
         |                             +- Apply
         |                             +- TypeApply
         |                             +- Super
         |                             +- Typed
         |                             +- Assign
         |                             +- Block
         |                             +- Lambda
         |                             +- If
         |                             +- Match
         |                             +- ImpliedMatch
         |                             +- Try
         |                             +- Return
         |                             +- Repeated
         |                             +- Inlined
         |                             +- SelectOuter
         |                             +- While
         |
         |
         +- TypeTree ----+- Inferred
         |               +- TypeIdent
         |               +- TypeSelect
         |               +- Projection
         |               +- Singleton
         |               +- Refined
         |               +- Applied
         |               +- Annotated
         |               +- MatchTypeTree
         |               +- ByName
         |               +- LambdaTypeTree
         |               +- TypeBind
         |               +- TypeBlock
         |
         +- TypeBoundsTree
         +- WildcardTypeTree
         +- CaseDef
         +- TypeCaseDef
+- Pattern --+- Value
             +- Bind
             +- Unapply
             +- Alternatives
             +- TypeTest
             +- WildcardPattern
                 +- NoPrefix
+- TypeOrBounds -+- TypeBounds
                 |
                 +- Type -------+- ConstantType
                                +- SymRef
                                +- TermRef
                                +- TypeRef
                                +- SuperType
                                +- Refinement
                                +- AppliedType
                                +- AnnotatedType
                                +- AndType
                                +- OrType
                                +- MatchType
                                +- ByNameType
                                +- ParamRef
                                +- ThisType
                                +- RecursiveThis
                                +- RecursiveType
                                +- LambdaType[ParamInfo <: TypeOrBounds] -+- MethodType
                                                                          +- PolyType
                                                                          +- TypeLambda
+- ImportSelector -+- SimpleSelector
                   +- RenameSelector
                   +- OmitSelector
+- Id
+- Signature
+- Position
+- Comment
+- Constant
+- Symbol --+- PackageDefSymbol
            |
            +- TypeSymbol -+- ClassDefSymbol
            |              +- TypeDefSymbol
            |              +- TypeBindSymbol
            |
            +- TermSymbol -+- DefDefSymbol
            |              +- ValDefSymbol
            |              +- BindSymbol
            |
            +- NoSymbol
+- Flags
```
```

<pre><code class="language-scala" >trait <a href="./ContextOps.md">ContextOps</a></pre></code>
<pre><code class="language-scala" >trait <a href="./ReportingOps.md">ReportingOps</a></pre></code>
<pre><code class="language-scala" >trait <a href="./ConstantOps.md">ConstantOps</a></pre></code>
<pre><code class="language-scala" >trait <a href="./Core.md">Core</a></pre></code>
Tasty reflect abstract types
```none```
+- Tree -+- PackageClause
         +- Import
         +- Statement -+- Definition --+- PackageDef
         |             |               +- ClassDef
         |             |               +- TypeDef
         |             |               +- DefDef
         |             |               +- ValDef
         |             |
         |             +- Term --------+- Ref -+- Ident
         |                             |       +- Select
         |                             |
         |                             +- Literal
         |                             +- This
         |                             +- New
         |                             +- NamedArg
         |                             +- Apply
         |                             +- TypeApply
         |                             +- Super
         |                             +- Typed
         |                             +- Assign
         |                             +- Block
         |                             +- Lambda
         |                             +- If
         |                             +- Match
         |                             +- ImpliedMatch
         |                             +- Try
         |                             +- Return
         |                             +- Repeated
         |                             +- Inlined
         |                             +- SelectOuter
         |                             +- While
         |
         |
         +- TypeTree ----+- Inferred
         |               +- TypeIdent
         |               +- TypeSelect
         |               +- Projection
         |               +- Singleton
         |               +- Refined
         |               +- Applied
         |               +- Annotated
         |               +- MatchTypeTree
         |               +- ByName
         |               +- LambdaTypeTree
         |               +- TypeBind
         |               +- TypeBlock
         |
         +- TypeBoundsTree
         +- WildcardTypeTree
         +- CaseDef
         +- TypeCaseDef
+- Pattern --+- Value
             +- Bind
             +- Unapply
             +- Alternatives
             +- TypeTest
             +- WildcardPattern
                 +- NoPrefix
+- TypeOrBounds -+- TypeBounds
                 |
                 +- Type -------+- ConstantType
                                +- SymRef
                                +- TermRef
                                +- TypeRef
                                +- SuperType
                                +- Refinement
                                +- AppliedType
                                +- AnnotatedType
                                +- AndType
                                +- OrType
                                +- MatchType
                                +- ByNameType
                                +- ParamRef
                                +- ThisType
                                +- RecursiveThis
                                +- RecursiveType
                                +- LambdaType[ParamInfo <: TypeOrBounds] -+- MethodType
                                                                          +- PolyType
                                                                          +- TypeLambda
+- ImportSelector -+- SimpleSelector
                   +- RenameSelector
                   +- OmitSelector
+- Id
+- Signature
+- Position
+- Comment
+- Constant
+- Symbol --+- PackageDefSymbol
            |
            +- TypeSymbol -+- ClassDefSymbol
            |              +- TypeDefSymbol
            |              +- TypeBindSymbol
            |
            +- TermSymbol -+- DefDefSymbol
            |              +- ValDefSymbol
            |              +- BindSymbol
            |
            +- NoSymbol
+- Flags
```
```

<pre><code class="language-scala" >trait <a href="./SignatureOps.md">SignatureOps</a></pre></code>
<pre><code class="language-scala" >class <a href="./ExprCastError.md">ExprCastError</a></pre></code>
<pre><code class="language-scala" >trait <a href="./PositionOps.md">PositionOps</a></pre></code>
<pre><code class="language-scala" >trait <a href="./IdOps.md">IdOps</a></pre></code>
<pre><code class="language-scala" >trait <a href="./Printers.md">Printers</a></pre></code>
<pre><code class="language-scala" >trait <a href="./PatternOps.md">PatternOps</a></pre></code>
<pre><code class="language-scala" >trait <a href="./RootPosition.md">RootPosition</a></pre></code>
<pre><code class="language-scala" >trait <a href="./FlagsOps.md">FlagsOps</a></pre></code>
<pre><code class="language-scala" >trait <a href="./TypeOrBoundsOps.md">TypeOrBoundsOps</a></pre></code>
<pre><code class="language-scala" >trait <a href="./TreeOps.md">TreeOps</a></pre></code>
<pre><code class="language-scala" >trait <a href="./StandardDefinitions.md">StandardDefinitions</a></pre></code>
<pre><code class="language-scala" >trait <a href="./SymbolOps.md">SymbolOps</a></pre></code>
Tasty reflect symbol

<pre><code class="language-scala" >trait <a href="./CommentOps.md">CommentOps</a></pre></code>
