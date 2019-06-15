scala.tasty.reflect
# trait PatternOps

<pre><code class="language-scala" >trait PatternOps extends Core</pre></code>
## Known subclasses:
<a href="./PatternOps/Pattern$.md">Pattern</a>, <a href="./PatternOps/PatternAPI.md">PatternAPI</a>, <a href="./PatternOps/TypeTestAPI.md">TypeTestAPI</a>, <a href="./PatternOps/AlternativesAPI.md">AlternativesAPI</a>, <a href="./PatternOps/UnapplyAPI.md">UnapplyAPI</a>, <a href="./PatternOps/BindAPI.md">BindAPI</a>, <a href="./PatternOps/ValueAPI.md">ValueAPI</a>
## Constructors:
<pre><code class="language-scala" >PatternOps()</pre></code>

## Concrete Type Members:
### Pattern
<pre><code class="language-scala" >final object <a href="./PatternOps/Pattern$.md">Pattern</a></pre></code>
### AlternativesAPI
<pre><code class="language-scala" >class <a href="./PatternOps/AlternativesAPI.md">AlternativesAPI</a></pre></code>
### BindAPI
<pre><code class="language-scala" >class <a href="./PatternOps/BindAPI.md">BindAPI</a></pre></code>
### PatternAPI
<pre><code class="language-scala" >class <a href="./PatternOps/PatternAPI.md">PatternAPI</a></pre></code>
### TypeTestAPI
<pre><code class="language-scala" >class <a href="./PatternOps/TypeTestAPI.md">TypeTestAPI</a></pre></code>
### UnapplyAPI
<pre><code class="language-scala" >class <a href="./PatternOps/UnapplyAPI.md">UnapplyAPI</a></pre></code>
### ValueAPI
<pre><code class="language-scala" >class <a href="./PatternOps/ValueAPI.md">ValueAPI</a></pre></code>
## Concrete Value Members:
### AlternativesAPI
<pre><code class="language-scala" >final implicit def AlternativesAPI(alternatives: Alternatives): AlternativesAPI</pre></code>

### BindAPI
<pre><code class="language-scala" >final implicit def BindAPI(bind: Bind): BindAPI</pre></code>

### PatternAPI
<pre><code class="language-scala" >final implicit def PatternAPI(self: Pattern): PatternAPI</pre></code>

### TypeTestAPI
<pre><code class="language-scala" >final implicit def TypeTestAPI(typeTest: TypeTest): TypeTestAPI</pre></code>

### UnapplyAPI
<pre><code class="language-scala" >final implicit def UnapplyAPI(unapply: Unapply): UnapplyAPI</pre></code>

### ValueAPI
<pre><code class="language-scala" >final implicit def ValueAPI(value: Value): ValueAPI</pre></code>

### Pattern
<pre><code class="language-scala" >final val Pattern: Pattern</pre></code>

