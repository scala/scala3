scala.tasty.util
# object Chars

<pre><code class="language-scala" >final object Chars extends Serializable</pre></code>
Contains constants and classifier methods for characters

## Concrete Value Members:
### !=
<pre><code class="language-scala" >final def !=(x$0: Any): Boolean</pre></code>

### ##
<pre><code class="language-scala" >final def ##: Int</pre></code>

### ==
<pre><code class="language-scala" >final def ==(x$0: Any): Boolean</pre></code>

### asInstanceOf
<pre><code class="language-scala" >final def asInstanceOf[X0]: X0</pre></code>

### char2uescape
<pre><code class="language-scala" >def char2uescape(c: Char): String</pre></code>
Convert a character to a backslash-u escape

### clone
<pre><code class="language-scala" >protected def clone(): Object</pre></code>

### digit2int
<pre><code class="language-scala" >def digit2int(ch: Char, base: Int): Int</pre></code>
Convert a character digit to an Int according to given base,
-1 if no success

### eq
<pre><code class="language-scala" >final def eq(x$0: Object): Boolean</pre></code>

### equals
<pre><code class="language-scala" >def equals(x$0: Any): Boolean</pre></code>

### finalize
<pre><code class="language-scala" >protected def finalize(): Unit</pre></code>

### getClass
<pre><code class="language-scala" >final def getClass(): Class[Nothing <: Any]</pre></code>

### hashCode
<pre><code class="language-scala" >def hashCode(): Int</pre></code>

### isIdentifierPart
<pre><code class="language-scala" >def isIdentifierPart(c: Char): Boolean</pre></code>
Can character form part of an alphanumeric Scala identifier?

### isIdentifierStart
<pre><code class="language-scala" >def isIdentifierStart(c: Char): Boolean</pre></code>
Can character start an alphanumeric Scala identifier?

### isInstanceOf
<pre><code class="language-scala" >final def isInstanceOf[X0]: Boolean</pre></code>

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

### ne
<pre><code class="language-scala" >final def ne(x$0: Object): Boolean</pre></code>

### notify
<pre><code class="language-scala" >final def notify(): Unit</pre></code>

### notifyAll
<pre><code class="language-scala" >final def notifyAll(): Unit</pre></code>

### synchronized
<pre><code class="language-scala" >final def synchronized[X0](x$0: X0): X0</pre></code>

### toString
<pre><code class="language-scala" >def toString(): String</pre></code>

### wait
<pre><code class="language-scala" >final def wait(x$0: Long, x$1: Int): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(x$0: Long): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(): Unit</pre></code>

### CR
<pre><code class="language-scala" >final inline val CR: </pre></code>

### FF
<pre><code class="language-scala" >final inline val FF: </pre></code>

### LF
<pre><code class="language-scala" >final inline val LF: 
</pre></code>

### SU
<pre><code class="language-scala" >final inline val SU: </pre></code>

