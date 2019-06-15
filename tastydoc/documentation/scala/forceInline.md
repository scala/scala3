scala
# class forceInline

<pre><code class="language-scala" >class forceInline extends Annotation with <a href="./annotation/StaticAnnotation.md">StaticAnnotation</a></pre></code>
An annotation on methods that is equivalent to Dotty `inline` modifier,
except that it does not imply `erased`.
The annotation should be used instead of the `inline` modifier in code
that needs to cross compile between Scala 2 and Dotty.
Note that Scala 2 ignores the `@forceInLine` annotation, and one must use
both the `@inline` and `@forceInline` annotation to inline across the
two compilers. E.g.
```scala```

## Constructors:
<pre><code class="language-scala" >forceInline()</pre></code>

