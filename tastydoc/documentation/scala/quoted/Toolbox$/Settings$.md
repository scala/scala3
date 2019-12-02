scala.quoted.Toolbox$
# object Settings

## Companion case class <a href="./Settings.md">Settings</a>

<pre><code class="language-scala" >final object Settings extends Serializable with Product</pre></code>
## Concrete Value Members:
### default
<pre><code class="language-scala" >implicit def default: <a href="./Settings.md">Settings</a></pre></code>

### make
<pre><code class="language-scala" >def make(color: Boolean, showRawTree: Boolean, outDir: Option[String], compilerArgs: List[String]): <a href="./Settings.md">Settings</a></pre></code>
Make toolbox settings

***showRawTree*** Do not remove quote tree artifacts

***compilerArgs*** Compiler arguments. Use only if you know what you are doing.

***color*** Print output with colors

***outDir*** Output directory for the compiled quote. If set to None the output will be in memory

