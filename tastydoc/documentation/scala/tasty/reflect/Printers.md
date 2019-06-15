scala.tasty.reflect
# trait Printers

<pre><code class="language-scala" >trait Printers extends Core with ConstantOps with FlagsOps with IdOps with ImportSelectorOps with PatternOps with PositionOps with SignatureOps with StandardDefinitions with SymbolOps with TreeOps with TypeOrBoundsOps</pre></code>
## Constructors:
<pre><code class="language-scala" >Printers()</pre></code>

## Abstract Type Members:
### Printer
<pre><code class="language-scala" >abstract class <a href="./Printers/Printer.md">Printer</a></pre></code>
## Concrete Type Members:
### ConstantShowDeco
<pre><code class="language-scala" >class <a href="./Printers/ConstantShowDeco.md">ConstantShowDeco</a></pre></code>
Adds `show` as an extension method of a `Constant`

### ExtractorsPrinter
<pre><code class="language-scala" >class <a href="./Printers/ExtractorsPrinter.md">ExtractorsPrinter</a></pre></code>
### FlagsShowDeco
<pre><code class="language-scala" >class <a href="./Printers/FlagsShowDeco.md">FlagsShowDeco</a></pre></code>
Adds `show` as an extension method of a `Flags`

### PatternShowDeco
<pre><code class="language-scala" >class <a href="./Printers/PatternShowDeco.md">PatternShowDeco</a></pre></code>
Adds `show` as an extension method of a `Pattern`

### SourceCodePrinter
<pre><code class="language-scala" >class <a href="./Printers/SourceCodePrinter.md">SourceCodePrinter</a></pre></code>
### SymbolShowDeco
<pre><code class="language-scala" >class <a href="./Printers/SymbolShowDeco.md">SymbolShowDeco</a></pre></code>
Adds `show` as an extension method of a `Symbol`

### TreeShowDeco
<pre><code class="language-scala" >class <a href="./Printers/TreeShowDeco.md">TreeShowDeco</a></pre></code>
Adds `show` as an extension method of a `Tree`

### TypeOrBoundsShowDeco
<pre><code class="language-scala" >class <a href="./Printers/TypeOrBoundsShowDeco.md">TypeOrBoundsShowDeco</a></pre></code>
Adds `show` as an extension method of a `TypeOrBounds`

## Concrete Value Members:
### typeOf
<pre><code class="language-scala" >def typeOf[T](evidence$2: <a href="../../quoted/Type.md">Type</a>[T]): Type</pre></code>

