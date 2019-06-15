scala
# object deriving

<pre><code class="language-scala" >final object deriving extends Serializable</pre></code>
## Concrete Type Members:
### EmptyProduct
<pre><code class="language-scala" >final object EmptyProduct</pre></code>
The empty product

### Mirror
<pre><code class="language-scala" >final object Mirror</pre></code>
### ArrayProduct
<pre><code class="language-scala" >class ArrayProduct</pre></code>
Helper class to turn arrays into products

### Mirror
<pre><code class="language-scala" >sealed trait Mirror</pre></code>
Mirrors allows typelevel access to enums, case classes and objects, and their sealed parents.

## Concrete Value Members:
### productElement
<pre><code class="language-scala" >def productElement[T](x: Any, idx: Int): T</pre></code>
Helper method to select a product element

