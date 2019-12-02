scala.util.control
# object NonLocalReturns

<pre><code class="language-scala" >final object NonLocalReturns extends Serializable</pre></code>
Library implementation of nonlocal return.
Usage:
   import scala.util.control.NonLocalReturns._
   returning { ... throwReturn(x) ... }

## Concrete Type Members:
### ReturnThrowable
<pre><code class="language-scala" >class ReturnThrowable</pre></code>
## Concrete Value Members:
### returning
<pre><code class="language-scala" >def returning[T](op: ImplicitFunction1[ReturnThrowable[T], T]): T</pre></code>
Enable nonlocal returns in `op`.

### throwReturn
<pre><code class="language-scala" >def throwReturn[T](result: T)(returner: ReturnThrowable[T]): Nothing</pre></code>
Performs a nonlocal return by throwing an exception.

