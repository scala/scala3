scala.quoted
# object Liftable

## Companion class <a href="./Liftable.md">Liftable</a>

<pre><code class="language-scala" >final object Liftable extends Serializable</pre></code>
Some liftable base types. To be completed with at least all types
that are valid Scala literals. The actual implementation of these
typed could be in terms of `ast.tpd.Literal`; the test `quotable.scala`
gives an alternative implementation using just the basic staging system.

## Known subclasses:
<a href="./Liftable$/PrimitiveLiftable.md">PrimitiveLiftable</a>
## Concrete Value Members:
### ClassIsLiftable
<pre><code class="language-scala" >implicit def ClassIsLiftable[T]: <a href="./Liftable.md">Liftable</a>[Class[T]]</pre></code>

### Liftable_Boolean_delegate
<pre><code class="language-scala" >implicit val Liftable_Boolean_delegate: <a href="./Liftable.md">Liftable</a>[Boolean]</pre></code>

### Liftable_Char_delegate
<pre><code class="language-scala" >implicit val Liftable_Char_delegate: <a href="./Liftable.md">Liftable</a>[Char]</pre></code>

### Liftable_Double_delegate
<pre><code class="language-scala" >implicit val Liftable_Double_delegate: <a href="./Liftable.md">Liftable</a>[Double]</pre></code>

### Liftable_Float_delegate
<pre><code class="language-scala" >implicit val Liftable_Float_delegate: <a href="./Liftable.md">Liftable</a>[Float]</pre></code>

### Liftable_Int_delegate
<pre><code class="language-scala" >implicit val Liftable_Int_delegate: <a href="./Liftable.md">Liftable</a>[Int]</pre></code>

### Liftable_Long_delegate
<pre><code class="language-scala" >implicit val Liftable_Long_delegate: <a href="./Liftable.md">Liftable</a>[Long]</pre></code>

### Liftable_Short_delegate
<pre><code class="language-scala" >implicit val Liftable_Short_delegate: <a href="./Liftable.md">Liftable</a>[Short]</pre></code>

### Liftable_String_delegate
<pre><code class="language-scala" >implicit val Liftable_String_delegate: <a href="./Liftable.md">Liftable</a>[String]</pre></code>

