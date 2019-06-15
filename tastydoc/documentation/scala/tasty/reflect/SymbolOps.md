scala.tasty.reflect
# trait SymbolOps

<pre><code class="language-scala" >trait SymbolOps extends Core</pre></code>
Tasty reflect symbol

## Known subclasses:
<a href="./SymbolOps/NoSymbol$.md">NoSymbol</a>, <a href="./SymbolOps/BindSymbolAPI.md">BindSymbolAPI</a>, <a href="./SymbolOps/IsBindSymbol$.md">IsBindSymbol</a>, <a href="./SymbolOps/ValDefSymbolAPI.md">ValDefSymbolAPI</a>, <a href="./SymbolOps/IsValDefSymbol$.md">IsValDefSymbol</a>, <a href="./SymbolOps/DefDefSymbolAPI.md">DefDefSymbolAPI</a>, <a href="./SymbolOps/IsDefDefSymbol$.md">IsDefDefSymbol</a>, <a href="./SymbolOps/IsTermSymbol$.md">IsTermSymbol</a>, <a href="./SymbolOps/TypeBindSymbolAPI.md">TypeBindSymbolAPI</a>, <a href="./SymbolOps/IsTypeBindSymbol$.md">IsTypeBindSymbol</a>, <a href="./SymbolOps/TypeDefSymbolAPI.md">TypeDefSymbolAPI</a>, <a href="./SymbolOps/IsTypeDefSymbol$.md">IsTypeDefSymbol</a>, <a href="./SymbolOps/ClassDefSymbolAPI.md">ClassDefSymbolAPI</a>, <a href="./SymbolOps/ClassDefSymbol$.md">ClassDefSymbol</a>, <a href="./SymbolOps/IsClassDefSymbol$.md">IsClassDefSymbol</a>, <a href="./SymbolOps/IsTypeSymbol$.md">IsTypeSymbol</a>, <a href="./SymbolOps/PackageDefSymbolAPI.md">PackageDefSymbolAPI</a>, <a href="./SymbolOps/IsPackageDefSymbol$.md">IsPackageDefSymbol</a>, <a href="./SymbolOps/SymbolAPI.md">SymbolAPI</a>
## Constructors:
<pre><code class="language-scala" >SymbolOps()</pre></code>

## Concrete Type Members:
### ClassDefSymbol
<pre><code class="language-scala" >final object <a href="./SymbolOps/ClassDefSymbol$.md">ClassDefSymbol</a></pre></code>
### IsBindSymbol
<pre><code class="language-scala" >final object <a href="./SymbolOps/IsBindSymbol$.md">IsBindSymbol</a></pre></code>
### IsClassDefSymbol
<pre><code class="language-scala" >final object <a href="./SymbolOps/IsClassDefSymbol$.md">IsClassDefSymbol</a></pre></code>
### IsDefDefSymbol
<pre><code class="language-scala" >final object <a href="./SymbolOps/IsDefDefSymbol$.md">IsDefDefSymbol</a></pre></code>
### IsPackageDefSymbol
<pre><code class="language-scala" >final object <a href="./SymbolOps/IsPackageDefSymbol$.md">IsPackageDefSymbol</a></pre></code>
### IsTermSymbol
<pre><code class="language-scala" >final object <a href="./SymbolOps/IsTermSymbol$.md">IsTermSymbol</a></pre></code>
### IsTypeBindSymbol
<pre><code class="language-scala" >final object <a href="./SymbolOps/IsTypeBindSymbol$.md">IsTypeBindSymbol</a></pre></code>
### IsTypeDefSymbol
<pre><code class="language-scala" >final object <a href="./SymbolOps/IsTypeDefSymbol$.md">IsTypeDefSymbol</a></pre></code>
### IsTypeSymbol
<pre><code class="language-scala" >final object <a href="./SymbolOps/IsTypeSymbol$.md">IsTypeSymbol</a></pre></code>
### IsValDefSymbol
<pre><code class="language-scala" >final object <a href="./SymbolOps/IsValDefSymbol$.md">IsValDefSymbol</a></pre></code>
### NoSymbol
<pre><code class="language-scala" >final object <a href="./SymbolOps/NoSymbol$.md">NoSymbol</a></pre></code>
### BindSymbolAPI
<pre><code class="language-scala" >class <a href="./SymbolOps/BindSymbolAPI.md">BindSymbolAPI</a></pre></code>
### ClassDefSymbolAPI
<pre><code class="language-scala" >class <a href="./SymbolOps/ClassDefSymbolAPI.md">ClassDefSymbolAPI</a></pre></code>
### DefDefSymbolAPI
<pre><code class="language-scala" >class <a href="./SymbolOps/DefDefSymbolAPI.md">DefDefSymbolAPI</a></pre></code>
### PackageDefSymbolAPI
<pre><code class="language-scala" >class <a href="./SymbolOps/PackageDefSymbolAPI.md">PackageDefSymbolAPI</a></pre></code>
### SymbolAPI
<pre><code class="language-scala" >class <a href="./SymbolOps/SymbolAPI.md">SymbolAPI</a></pre></code>
### TypeBindSymbolAPI
<pre><code class="language-scala" >class <a href="./SymbolOps/TypeBindSymbolAPI.md">TypeBindSymbolAPI</a></pre></code>
### TypeDefSymbolAPI
<pre><code class="language-scala" >class <a href="./SymbolOps/TypeDefSymbolAPI.md">TypeDefSymbolAPI</a></pre></code>
### ValDefSymbolAPI
<pre><code class="language-scala" >class <a href="./SymbolOps/ValDefSymbolAPI.md">ValDefSymbolAPI</a></pre></code>
## Concrete Value Members:
### BindSymbolAPI
<pre><code class="language-scala" >final implicit def BindSymbolAPI(self: BindSymbol): BindSymbolAPI</pre></code>

### ClassDefSymbolAPI
<pre><code class="language-scala" >final implicit def ClassDefSymbolAPI(self: ClassDefSymbol): ClassDefSymbolAPI</pre></code>

### DefDefSymbolAPI
<pre><code class="language-scala" >final implicit def DefDefSymbolAPI(self: DefDefSymbol): DefDefSymbolAPI</pre></code>

### PackageDefSymbolAPI
<pre><code class="language-scala" >final implicit def PackageDefSymbolAPI(self: PackageDefSymbol): PackageDefSymbolAPI</pre></code>

### SymbolAPI
<pre><code class="language-scala" >final implicit def SymbolAPI(self: Symbol): SymbolAPI</pre></code>

### TypeBindSymbolAPI
<pre><code class="language-scala" >final implicit def TypeBindSymbolAPI(self: TypeBindSymbol): TypeBindSymbolAPI</pre></code>

### TypeDefSymbolAPI
<pre><code class="language-scala" >final implicit def TypeDefSymbolAPI(self: TypeDefSymbol): TypeDefSymbolAPI</pre></code>

### ValDefSymbolAPI
<pre><code class="language-scala" >final implicit def ValDefSymbolAPI(self: ValDefSymbol): ValDefSymbolAPI</pre></code>

### ClassDefSymbol
<pre><code class="language-scala" >final val ClassDefSymbol: ClassDefSymbol</pre></code>

### IsBindSymbol
<pre><code class="language-scala" >final val IsBindSymbol: IsBindSymbol</pre></code>

### IsClassDefSymbol
<pre><code class="language-scala" >final val IsClassDefSymbol: IsClassDefSymbol</pre></code>

### IsDefDefSymbol
<pre><code class="language-scala" >final val IsDefDefSymbol: IsDefDefSymbol</pre></code>

### IsPackageDefSymbol
<pre><code class="language-scala" >final val IsPackageDefSymbol: IsPackageDefSymbol</pre></code>

### IsTermSymbol
<pre><code class="language-scala" >final val IsTermSymbol: IsTermSymbol</pre></code>

### IsTypeBindSymbol
<pre><code class="language-scala" >final val IsTypeBindSymbol: IsTypeBindSymbol</pre></code>

### IsTypeDefSymbol
<pre><code class="language-scala" >final val IsTypeDefSymbol: IsTypeDefSymbol</pre></code>

### IsTypeSymbol
<pre><code class="language-scala" >final val IsTypeSymbol: IsTypeSymbol</pre></code>

### IsValDefSymbol
<pre><code class="language-scala" >final val IsValDefSymbol: IsValDefSymbol</pre></code>

### NoSymbol
<pre><code class="language-scala" >final val NoSymbol: NoSymbol</pre></code>

