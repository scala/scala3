scala.quoted.Toolbox$
# object Settings

## Companion case class <a href="./Settings.md">Settings</a>

<pre><code class="language-scala" >final object Settings extends Serializable with Product</pre></code>
## Concrete Type Members:
### MirroredMonoType
<pre><code class="language-scala" >type MirroredMonoType: <a href="./Settings.md">Settings</a></pre></code>

## Concrete Value Members:
### default
<pre><code class="language-scala" >implicit def default: <a href="./Settings.md">Settings</a></pre></code>

### fromProduct
<pre><code class="language-scala" >def fromProduct(x$0: Product): <a href="./Settings$.md#MirroredMonoType">MirroredMonoType</a></pre></code>

### make
<pre><code class="language-scala" >def make(color: Boolean, showRawTree: Boolean, outDir: Option[String], compilerArgs: List[String]): <a href="./Settings.md">Settings</a></pre></code>
Make toolbox settings

***showRawTree*** Do not remove quote tree artifacts

***compilerArgs*** Compiler arguments. Use only if you know what you are doing.

***color*** Print output with colors

***outDir*** Output directory for the compiled quote. If set to None the output will be in memory

### make$default$1
<pre><code class="language-scala" >def make$default$1: Boolean</pre></code>
Make toolbox settings

***showRawTree*** Do not remove quote tree artifacts

***compilerArgs*** Compiler arguments. Use only if you know what you are doing.

***color*** Print output with colors

***outDir*** Output directory for the compiled quote. If set to None the output will be in memory

### make$default$2
<pre><code class="language-scala" >def make$default$2: Boolean</pre></code>
Make toolbox settings

***showRawTree*** Do not remove quote tree artifacts

***compilerArgs*** Compiler arguments. Use only if you know what you are doing.

***color*** Print output with colors

***outDir*** Output directory for the compiled quote. If set to None the output will be in memory

### make$default$3
<pre><code class="language-scala" >def make$default$3: None</pre></code>
Make toolbox settings

***showRawTree*** Do not remove quote tree artifacts

***compilerArgs*** Compiler arguments. Use only if you know what you are doing.

***color*** Print output with colors

***outDir*** Output directory for the compiled quote. If set to None the output will be in memory

### make$default$4
<pre><code class="language-scala" >def make$default$4: Nil</pre></code>
Make toolbox settings

***showRawTree*** Do not remove quote tree artifacts

***compilerArgs*** Compiler arguments. Use only if you know what you are doing.

***color*** Print output with colors

***outDir*** Output directory for the compiled quote. If set to None the output will be in memory

### unapply
<pre><code class="language-scala" >def unapply(x$1: <a href="./Settings.md">Settings</a>): <a href="./Settings.md">Settings</a></pre></code>

