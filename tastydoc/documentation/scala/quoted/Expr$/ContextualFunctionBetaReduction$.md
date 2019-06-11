scala.quoted.Expr$
# object ContextualFunctionBetaReduction

<pre><code class="language-scala" >final object ContextualFunctionBetaReduction extends Serializable</pre></code>
## Concrete Value Members:
### !=
<pre><code class="language-scala" >final def !=(x$0: Any): Boolean</pre></code>

### ##
<pre><code class="language-scala" >final def ##: Int</pre></code>

### ==
<pre><code class="language-scala" >final def ==(x$0: Any): Boolean</pre></code>

### apply
<pre><code class="language-scala" >def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R](f: <a href="../Expr.md">Expr</a>[ImplicitFunction22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R]])(x1: <a href="../Expr.md">Expr</a>[T1], x2: <a href="../Expr.md">Expr</a>[T2], x3: <a href="../Expr.md">Expr</a>[T3], x4: <a href="../Expr.md">Expr</a>[T4], x5: <a href="../Expr.md">Expr</a>[T5], x6: <a href="../Expr.md">Expr</a>[T6], x7: <a href="../Expr.md">Expr</a>[T7], x8: <a href="../Expr.md">Expr</a>[T8], x9: <a href="../Expr.md">Expr</a>[T9], x10: <a href="../Expr.md">Expr</a>[T10], x11: <a href="../Expr.md">Expr</a>[T11], x12: <a href="../Expr.md">Expr</a>[T12], x13: <a href="../Expr.md">Expr</a>[T13], x14: <a href="../Expr.md">Expr</a>[T14], x15: <a href="../Expr.md">Expr</a>[T15], x16: <a href="../Expr.md">Expr</a>[T16], x17: <a href="../Expr.md">Expr</a>[T17], x18: <a href="../Expr.md">Expr</a>[T18], x19: <a href="../Expr.md">Expr</a>[T19], x20: <a href="../Expr.md">Expr</a>[T20], x21: <a href="../Expr.md">Expr</a>[T21], x22: <a href="../Expr.md">Expr</a>[T22]): <a href="../Expr.md">Expr</a>[R]</pre></code>
Beta-reduces the contextual function appication.
Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments

### apply
<pre><code class="language-scala" >def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R](f: <a href="../Expr.md">Expr</a>[ImplicitFunction21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R]])(x1: <a href="../Expr.md">Expr</a>[T1], x2: <a href="../Expr.md">Expr</a>[T2], x3: <a href="../Expr.md">Expr</a>[T3], x4: <a href="../Expr.md">Expr</a>[T4], x5: <a href="../Expr.md">Expr</a>[T5], x6: <a href="../Expr.md">Expr</a>[T6], x7: <a href="../Expr.md">Expr</a>[T7], x8: <a href="../Expr.md">Expr</a>[T8], x9: <a href="../Expr.md">Expr</a>[T9], x10: <a href="../Expr.md">Expr</a>[T10], x11: <a href="../Expr.md">Expr</a>[T11], x12: <a href="../Expr.md">Expr</a>[T12], x13: <a href="../Expr.md">Expr</a>[T13], x14: <a href="../Expr.md">Expr</a>[T14], x15: <a href="../Expr.md">Expr</a>[T15], x16: <a href="../Expr.md">Expr</a>[T16], x17: <a href="../Expr.md">Expr</a>[T17], x18: <a href="../Expr.md">Expr</a>[T18], x19: <a href="../Expr.md">Expr</a>[T19], x20: <a href="../Expr.md">Expr</a>[T20], x21: <a href="../Expr.md">Expr</a>[T21]): <a href="../Expr.md">Expr</a>[R]</pre></code>
Beta-reduces the contextual function appication.
Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments

### apply
<pre><code class="language-scala" >def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R](f: <a href="../Expr.md">Expr</a>[ImplicitFunction20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R]])(x1: <a href="../Expr.md">Expr</a>[T1], x2: <a href="../Expr.md">Expr</a>[T2], x3: <a href="../Expr.md">Expr</a>[T3], x4: <a href="../Expr.md">Expr</a>[T4], x5: <a href="../Expr.md">Expr</a>[T5], x6: <a href="../Expr.md">Expr</a>[T6], x7: <a href="../Expr.md">Expr</a>[T7], x8: <a href="../Expr.md">Expr</a>[T8], x9: <a href="../Expr.md">Expr</a>[T9], x10: <a href="../Expr.md">Expr</a>[T10], x11: <a href="../Expr.md">Expr</a>[T11], x12: <a href="../Expr.md">Expr</a>[T12], x13: <a href="../Expr.md">Expr</a>[T13], x14: <a href="../Expr.md">Expr</a>[T14], x15: <a href="../Expr.md">Expr</a>[T15], x16: <a href="../Expr.md">Expr</a>[T16], x17: <a href="../Expr.md">Expr</a>[T17], x18: <a href="../Expr.md">Expr</a>[T18], x19: <a href="../Expr.md">Expr</a>[T19], x20: <a href="../Expr.md">Expr</a>[T20]): <a href="../Expr.md">Expr</a>[R]</pre></code>
Beta-reduces the contextual function appication.
Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments

### apply
<pre><code class="language-scala" >def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R](f: <a href="../Expr.md">Expr</a>[ImplicitFunction19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R]])(x1: <a href="../Expr.md">Expr</a>[T1], x2: <a href="../Expr.md">Expr</a>[T2], x3: <a href="../Expr.md">Expr</a>[T3], x4: <a href="../Expr.md">Expr</a>[T4], x5: <a href="../Expr.md">Expr</a>[T5], x6: <a href="../Expr.md">Expr</a>[T6], x7: <a href="../Expr.md">Expr</a>[T7], x8: <a href="../Expr.md">Expr</a>[T8], x9: <a href="../Expr.md">Expr</a>[T9], x10: <a href="../Expr.md">Expr</a>[T10], x11: <a href="../Expr.md">Expr</a>[T11], x12: <a href="../Expr.md">Expr</a>[T12], x13: <a href="../Expr.md">Expr</a>[T13], x14: <a href="../Expr.md">Expr</a>[T14], x15: <a href="../Expr.md">Expr</a>[T15], x16: <a href="../Expr.md">Expr</a>[T16], x17: <a href="../Expr.md">Expr</a>[T17], x18: <a href="../Expr.md">Expr</a>[T18], x19: <a href="../Expr.md">Expr</a>[T19]): <a href="../Expr.md">Expr</a>[R]</pre></code>
Beta-reduces the contextual function appication.
Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments

### apply
<pre><code class="language-scala" >def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R](f: <a href="../Expr.md">Expr</a>[ImplicitFunction18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R]])(x1: <a href="../Expr.md">Expr</a>[T1], x2: <a href="../Expr.md">Expr</a>[T2], x3: <a href="../Expr.md">Expr</a>[T3], x4: <a href="../Expr.md">Expr</a>[T4], x5: <a href="../Expr.md">Expr</a>[T5], x6: <a href="../Expr.md">Expr</a>[T6], x7: <a href="../Expr.md">Expr</a>[T7], x8: <a href="../Expr.md">Expr</a>[T8], x9: <a href="../Expr.md">Expr</a>[T9], x10: <a href="../Expr.md">Expr</a>[T10], x11: <a href="../Expr.md">Expr</a>[T11], x12: <a href="../Expr.md">Expr</a>[T12], x13: <a href="../Expr.md">Expr</a>[T13], x14: <a href="../Expr.md">Expr</a>[T14], x15: <a href="../Expr.md">Expr</a>[T15], x16: <a href="../Expr.md">Expr</a>[T16], x17: <a href="../Expr.md">Expr</a>[T17], x18: <a href="../Expr.md">Expr</a>[T18]): <a href="../Expr.md">Expr</a>[R]</pre></code>
Beta-reduces the contextual function appication.
Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments

### apply
<pre><code class="language-scala" >def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R](f: <a href="../Expr.md">Expr</a>[ImplicitFunction17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R]])(x1: <a href="../Expr.md">Expr</a>[T1], x2: <a href="../Expr.md">Expr</a>[T2], x3: <a href="../Expr.md">Expr</a>[T3], x4: <a href="../Expr.md">Expr</a>[T4], x5: <a href="../Expr.md">Expr</a>[T5], x6: <a href="../Expr.md">Expr</a>[T6], x7: <a href="../Expr.md">Expr</a>[T7], x8: <a href="../Expr.md">Expr</a>[T8], x9: <a href="../Expr.md">Expr</a>[T9], x10: <a href="../Expr.md">Expr</a>[T10], x11: <a href="../Expr.md">Expr</a>[T11], x12: <a href="../Expr.md">Expr</a>[T12], x13: <a href="../Expr.md">Expr</a>[T13], x14: <a href="../Expr.md">Expr</a>[T14], x15: <a href="../Expr.md">Expr</a>[T15], x16: <a href="../Expr.md">Expr</a>[T16], x17: <a href="../Expr.md">Expr</a>[T17]): <a href="../Expr.md">Expr</a>[R]</pre></code>
Beta-reduces the contextual function appication.
Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments

### apply
<pre><code class="language-scala" >def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R](f: <a href="../Expr.md">Expr</a>[ImplicitFunction16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R]])(x1: <a href="../Expr.md">Expr</a>[T1], x2: <a href="../Expr.md">Expr</a>[T2], x3: <a href="../Expr.md">Expr</a>[T3], x4: <a href="../Expr.md">Expr</a>[T4], x5: <a href="../Expr.md">Expr</a>[T5], x6: <a href="../Expr.md">Expr</a>[T6], x7: <a href="../Expr.md">Expr</a>[T7], x8: <a href="../Expr.md">Expr</a>[T8], x9: <a href="../Expr.md">Expr</a>[T9], x10: <a href="../Expr.md">Expr</a>[T10], x11: <a href="../Expr.md">Expr</a>[T11], x12: <a href="../Expr.md">Expr</a>[T12], x13: <a href="../Expr.md">Expr</a>[T13], x14: <a href="../Expr.md">Expr</a>[T14], x15: <a href="../Expr.md">Expr</a>[T15], x16: <a href="../Expr.md">Expr</a>[T16]): <a href="../Expr.md">Expr</a>[R]</pre></code>
Beta-reduces the contextual function appication.
Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments

### apply
<pre><code class="language-scala" >def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R](f: <a href="../Expr.md">Expr</a>[ImplicitFunction15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R]])(x1: <a href="../Expr.md">Expr</a>[T1], x2: <a href="../Expr.md">Expr</a>[T2], x3: <a href="../Expr.md">Expr</a>[T3], x4: <a href="../Expr.md">Expr</a>[T4], x5: <a href="../Expr.md">Expr</a>[T5], x6: <a href="../Expr.md">Expr</a>[T6], x7: <a href="../Expr.md">Expr</a>[T7], x8: <a href="../Expr.md">Expr</a>[T8], x9: <a href="../Expr.md">Expr</a>[T9], x10: <a href="../Expr.md">Expr</a>[T10], x11: <a href="../Expr.md">Expr</a>[T11], x12: <a href="../Expr.md">Expr</a>[T12], x13: <a href="../Expr.md">Expr</a>[T13], x14: <a href="../Expr.md">Expr</a>[T14], x15: <a href="../Expr.md">Expr</a>[T15]): <a href="../Expr.md">Expr</a>[R]</pre></code>
Beta-reduces the contextual function appication.
Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments

### apply
<pre><code class="language-scala" >def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R](f: <a href="../Expr.md">Expr</a>[ImplicitFunction14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R]])(x1: <a href="../Expr.md">Expr</a>[T1], x2: <a href="../Expr.md">Expr</a>[T2], x3: <a href="../Expr.md">Expr</a>[T3], x4: <a href="../Expr.md">Expr</a>[T4], x5: <a href="../Expr.md">Expr</a>[T5], x6: <a href="../Expr.md">Expr</a>[T6], x7: <a href="../Expr.md">Expr</a>[T7], x8: <a href="../Expr.md">Expr</a>[T8], x9: <a href="../Expr.md">Expr</a>[T9], x10: <a href="../Expr.md">Expr</a>[T10], x11: <a href="../Expr.md">Expr</a>[T11], x12: <a href="../Expr.md">Expr</a>[T12], x13: <a href="../Expr.md">Expr</a>[T13], x14: <a href="../Expr.md">Expr</a>[T14]): <a href="../Expr.md">Expr</a>[R]</pre></code>
Beta-reduces the contextual function appication.
Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments

### apply
<pre><code class="language-scala" >def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R](f: <a href="../Expr.md">Expr</a>[ImplicitFunction13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R]])(x1: <a href="../Expr.md">Expr</a>[T1], x2: <a href="../Expr.md">Expr</a>[T2], x3: <a href="../Expr.md">Expr</a>[T3], x4: <a href="../Expr.md">Expr</a>[T4], x5: <a href="../Expr.md">Expr</a>[T5], x6: <a href="../Expr.md">Expr</a>[T6], x7: <a href="../Expr.md">Expr</a>[T7], x8: <a href="../Expr.md">Expr</a>[T8], x9: <a href="../Expr.md">Expr</a>[T9], x10: <a href="../Expr.md">Expr</a>[T10], x11: <a href="../Expr.md">Expr</a>[T11], x12: <a href="../Expr.md">Expr</a>[T12], x13: <a href="../Expr.md">Expr</a>[T13]): <a href="../Expr.md">Expr</a>[R]</pre></code>
Beta-reduces the contextual function appication.
Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments

### apply
<pre><code class="language-scala" >def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R](f: <a href="../Expr.md">Expr</a>[ImplicitFunction12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R]])(x1: <a href="../Expr.md">Expr</a>[T1], x2: <a href="../Expr.md">Expr</a>[T2], x3: <a href="../Expr.md">Expr</a>[T3], x4: <a href="../Expr.md">Expr</a>[T4], x5: <a href="../Expr.md">Expr</a>[T5], x6: <a href="../Expr.md">Expr</a>[T6], x7: <a href="../Expr.md">Expr</a>[T7], x8: <a href="../Expr.md">Expr</a>[T8], x9: <a href="../Expr.md">Expr</a>[T9], x10: <a href="../Expr.md">Expr</a>[T10], x11: <a href="../Expr.md">Expr</a>[T11], x12: <a href="../Expr.md">Expr</a>[T12]): <a href="../Expr.md">Expr</a>[R]</pre></code>
Beta-reduces the contextual function appication.
Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments

### apply
<pre><code class="language-scala" >def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R](f: <a href="../Expr.md">Expr</a>[ImplicitFunction11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R]])(x1: <a href="../Expr.md">Expr</a>[T1], x2: <a href="../Expr.md">Expr</a>[T2], x3: <a href="../Expr.md">Expr</a>[T3], x4: <a href="../Expr.md">Expr</a>[T4], x5: <a href="../Expr.md">Expr</a>[T5], x6: <a href="../Expr.md">Expr</a>[T6], x7: <a href="../Expr.md">Expr</a>[T7], x8: <a href="../Expr.md">Expr</a>[T8], x9: <a href="../Expr.md">Expr</a>[T9], x10: <a href="../Expr.md">Expr</a>[T10], x11: <a href="../Expr.md">Expr</a>[T11]): <a href="../Expr.md">Expr</a>[R]</pre></code>
Beta-reduces the contextual function appication.
Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments

### apply
<pre><code class="language-scala" >def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R](f: <a href="../Expr.md">Expr</a>[ImplicitFunction10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R]])(x1: <a href="../Expr.md">Expr</a>[T1], x2: <a href="../Expr.md">Expr</a>[T2], x3: <a href="../Expr.md">Expr</a>[T3], x4: <a href="../Expr.md">Expr</a>[T4], x5: <a href="../Expr.md">Expr</a>[T5], x6: <a href="../Expr.md">Expr</a>[T6], x7: <a href="../Expr.md">Expr</a>[T7], x8: <a href="../Expr.md">Expr</a>[T8], x9: <a href="../Expr.md">Expr</a>[T9], x10: <a href="../Expr.md">Expr</a>[T10]): <a href="../Expr.md">Expr</a>[R]</pre></code>
Beta-reduces the contextual function appication.
Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments

### apply
<pre><code class="language-scala" >def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, R](f: <a href="../Expr.md">Expr</a>[ImplicitFunction9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R]])(x1: <a href="../Expr.md">Expr</a>[T1], x2: <a href="../Expr.md">Expr</a>[T2], x3: <a href="../Expr.md">Expr</a>[T3], x4: <a href="../Expr.md">Expr</a>[T4], x5: <a href="../Expr.md">Expr</a>[T5], x6: <a href="../Expr.md">Expr</a>[T6], x7: <a href="../Expr.md">Expr</a>[T7], x8: <a href="../Expr.md">Expr</a>[T8], x9: <a href="../Expr.md">Expr</a>[T9]): <a href="../Expr.md">Expr</a>[R]</pre></code>
Beta-reduces the contextual function appication.
Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments

### apply
<pre><code class="language-scala" >def apply[T1, T2, T3, T4, T5, T6, T7, T8, R](f: <a href="../Expr.md">Expr</a>[ImplicitFunction8[T1, T2, T3, T4, T5, T6, T7, T8, R]])(x1: <a href="../Expr.md">Expr</a>[T1], x2: <a href="../Expr.md">Expr</a>[T2], x3: <a href="../Expr.md">Expr</a>[T3], x4: <a href="../Expr.md">Expr</a>[T4], x5: <a href="../Expr.md">Expr</a>[T5], x6: <a href="../Expr.md">Expr</a>[T6], x7: <a href="../Expr.md">Expr</a>[T7], x8: <a href="../Expr.md">Expr</a>[T8]): <a href="../Expr.md">Expr</a>[R]</pre></code>
Beta-reduces the contextual function appication.
Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments

### apply
<pre><code class="language-scala" >def apply[T1, T2, T3, T4, T5, T6, T7, R](f: <a href="../Expr.md">Expr</a>[ImplicitFunction7[T1, T2, T3, T4, T5, T6, T7, R]])(x1: <a href="../Expr.md">Expr</a>[T1], x2: <a href="../Expr.md">Expr</a>[T2], x3: <a href="../Expr.md">Expr</a>[T3], x4: <a href="../Expr.md">Expr</a>[T4], x5: <a href="../Expr.md">Expr</a>[T5], x6: <a href="../Expr.md">Expr</a>[T6], x7: <a href="../Expr.md">Expr</a>[T7]): <a href="../Expr.md">Expr</a>[R]</pre></code>
Beta-reduces the contextual function appication.
Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments

### apply
<pre><code class="language-scala" >def apply[T1, T2, T3, T4, T5, T6, R](f: <a href="../Expr.md">Expr</a>[ImplicitFunction6[T1, T2, T3, T4, T5, T6, R]])(x1: <a href="../Expr.md">Expr</a>[T1], x2: <a href="../Expr.md">Expr</a>[T2], x3: <a href="../Expr.md">Expr</a>[T3], x4: <a href="../Expr.md">Expr</a>[T4], x5: <a href="../Expr.md">Expr</a>[T5], x6: <a href="../Expr.md">Expr</a>[T6]): <a href="../Expr.md">Expr</a>[R]</pre></code>
Beta-reduces the contextual function appication.
Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments

### apply
<pre><code class="language-scala" >def apply[T1, T2, T3, T4, T5, R](f: <a href="../Expr.md">Expr</a>[ImplicitFunction5[T1, T2, T3, T4, T5, R]])(x1: <a href="../Expr.md">Expr</a>[T1], x2: <a href="../Expr.md">Expr</a>[T2], x3: <a href="../Expr.md">Expr</a>[T3], x4: <a href="../Expr.md">Expr</a>[T4], x5: <a href="../Expr.md">Expr</a>[T5]): <a href="../Expr.md">Expr</a>[R]</pre></code>
Beta-reduces the contextual function appication.
Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments

### apply
<pre><code class="language-scala" >def apply[T1, T2, T3, T4, R](f: <a href="../Expr.md">Expr</a>[ImplicitFunction4[T1, T2, T3, T4, R]])(x1: <a href="../Expr.md">Expr</a>[T1], x2: <a href="../Expr.md">Expr</a>[T2], x3: <a href="../Expr.md">Expr</a>[T3], x4: <a href="../Expr.md">Expr</a>[T4]): <a href="../Expr.md">Expr</a>[R]</pre></code>
Beta-reduces the contextual function appication.
Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments

### apply
<pre><code class="language-scala" >def apply[T1, T2, T3, R](f: <a href="../Expr.md">Expr</a>[ImplicitFunction3[T1, T2, T3, R]])(x1: <a href="../Expr.md">Expr</a>[T1], x2: <a href="../Expr.md">Expr</a>[T2], x3: <a href="../Expr.md">Expr</a>[T3]): <a href="../Expr.md">Expr</a>[R]</pre></code>
Beta-reduces the contextual function appication.
Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments

### apply
<pre><code class="language-scala" >def apply[T1, T2, R](f: <a href="../Expr.md">Expr</a>[ImplicitFunction2[T1, T2, R]])(x1: <a href="../Expr.md">Expr</a>[T1], x2: <a href="../Expr.md">Expr</a>[T2]): <a href="../Expr.md">Expr</a>[R]</pre></code>
Beta-reduces the contextual function appication.
Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments

### apply
<pre><code class="language-scala" >def apply[T1, R](f: <a href="../Expr.md">Expr</a>[ImplicitFunction1[T1, R]])(x1: <a href="../Expr.md">Expr</a>[T1]): <a href="../Expr.md">Expr</a>[R]</pre></code>
Beta-reduces the contextual function appication.
Generates the an expression that evaluates all arguments and then evaluates the body with the evaluated arguments

### asInstanceOf
<pre><code class="language-scala" >final def asInstanceOf[X0]: X0</pre></code>

### clone
<pre><code class="language-scala" >protected def clone(): Object</pre></code>

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

### isInstanceOf
<pre><code class="language-scala" >final def isInstanceOf[X0]: Boolean</pre></code>

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

