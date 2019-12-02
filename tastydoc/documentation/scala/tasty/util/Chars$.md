scala.tasty.util
# object Chars

<pre><code class="language-scala" >final object Chars extends Serializable</pre></code>
Contains constants and classifier methods for characters

## Concrete Value Members:
### char2uescape
<pre><code class="language-scala" >def char2uescape(c: Char): String</pre></code>
Convert a character to a backslash-u escape

### digit2int
<pre><code class="language-scala" >def digit2int(ch: Char, base: Int): Int</pre></code>
Convert a character digit to an Int according to given base,
-1 if no success

### isIdentifierPart
<pre><code class="language-scala" >def isIdentifierPart(c: Char): Boolean</pre></code>
Can character form part of an alphanumeric Scala identifier?

### isIdentifierStart
<pre><code class="language-scala" >def isIdentifierStart(c: Char): Boolean</pre></code>
Can character start an alphanumeric Scala identifier?

### isLineBreakChar
<pre><code class="language-scala" >def isLineBreakChar(c: Char): Boolean</pre></code>
Is character a line break?

### isOperatorPart
<pre><code class="language-scala" >def isOperatorPart(c: Char): Boolean</pre></code>
Can character form part of a Scala operator name?

### isScalaLetter
<pre><code class="language-scala" >def isScalaLetter(ch: Char): Boolean</pre></code>

### isSpecial
<pre><code class="language-scala" >def isSpecial(c: Char): Boolean</pre></code>
Is character a math or other symbol in Unicode?

### isValidJVMChar
<pre><code class="language-scala" >def isValidJVMChar(c: Char): Boolean</pre></code>

### isValidJVMMethodChar
<pre><code class="language-scala" >def isValidJVMMethodChar(c: Char): Boolean</pre></code>

### isVarPart
<pre><code class="language-scala" >def isVarPart(c: Char): Boolean</pre></code>
Can character form part of a doc comment variable $xxx?

### isWhitespace
<pre><code class="language-scala" >def isWhitespace(c: Char): Boolean</pre></code>
Is character a whitespace character (but not a new line)?

### CR
<pre><code class="language-scala" >final inline val CR: </pre></code>

### FF
<pre><code class="language-scala" >final inline val FF: </pre></code>

### LF
<pre><code class="language-scala" >final inline val LF: 
</pre></code>

### SU
<pre><code class="language-scala" >final inline val SU: </pre></code>

