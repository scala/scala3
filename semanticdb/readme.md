# SemanticDB

To run the tests use two terminals. In the first terminal compile the
`semanticdb/input` project whenever sources change in
`semanticdb/input/src/main/scala/**.scala`.

```
cd semanticdb/input
sbt
> compile
```

In the second terminal, run `sbt dotty-semanticdb/test` from the main dotty
build

```
sbt
> dotty-semanticdb/test
```

The tests assert that the TASTy to SemanticDB converter produces the same output
as the semanticdb-scalac compiler plugin. Test failures result in diffs like
this

```diff
Test dotty.semanticdb.Tests.testExample failed: java.lang.AssertionError:
--- tasty
+++ scala2
-class Example {
-  val a: String = "1"
+class Example /*example/Example#*/  {
+  val a /*example/Example#a.*/ : String /*scala/Predef.String#*/  = "1"
```

The lines starting with `-` are the output from the TASTy converter and the
lines starting with `+` are the output from the semanticdb-scalac compiler
plugin.

Once the TASTy converter is updated to emit correct `SymbolOccurrence`
(https://scalameta.org/docs/semanticdb/specification.html#symboloccurrence) then
the "tasty" output should become identical to "scala2" and include inline
comments `class Example /* pkg/Example# */`. The inline comments are read as:
"the symbol `pkg/Example#` was resolved next to the class name `Example`".

The spec for SemanticDB symbols can be found here:

- Scala symbols:
  https://scalameta.org/docs/semanticdb/specification.html#scala-symbol
- Java symbols:
  https://scalameta.org/docs/semanticdb/specification.html#java-symbol
