---
layout: doc-page
title: Optional Braces
---

As an experimental feature, Scala 3 enforces some rules on indentation and allows
some occurrences of braces `{...}` to be optional.

 - First, some badly indented programs are ruled out, which means they are flagged with warnings.
 - Second, some occurrences of braces `{...}` are made optional. Generally, the rule
   is that adding a pair of optional braces will not change the meaning of a well-indented program.

### Indentation Rules

The compiler enforces two rules for well-indented programs, flagging violations as warnings.

 1. In a brace-delimited region, no statement is allowed to start to the left
    of the first statement after the opening brace that starts a new line.

    This rule is helpful for finding missing closing braces. It prevents errors like:

    ```scala
    if (x < 0) {
      println(1)
      println(2)

    println("done")  // error: indented too far to the left
    ```

 2. If significant indentation is turned off (i.e. under Scala-2 mode or under `-noindent`) and we are at the  start of an indented sub-part of an expression, and the indented part ends in a newline, the next statement must start at an indentation width less than the sub-part. This prevents errors where an opening brace was forgotten, as in

    ```scala
    if (x < 0)
      println(1)
      println(2)   // error: missing `{`
    ```

These rules still leave a lot of leeway how programs should be indented. For instance, they do not impose
any restrictions on indentation within expressions, nor do they require that all statements of an indentation block line up exactly.

The rules are generally helpful in pinpointing the root cause of errors related to missing opening or closing braces. These errors are often quite hard to diagnose, in particular in large programs.

### Optional Braces

The compiler will insert `<indent>` or `<outdent>`
tokens at certain line breaks. Grammatically, pairs of `<indent>` and `<outdent>` tokens have the same effect as pairs of braces `{` and `}`.

The algorithm makes use of a stack `IW` of previously encountered indentation widths. The stack initially holds a single element with a zero indentation width. The _current indentation width_ is the indentation width of the top of the stack.

There are two rules:

 1. An `<indent>` is inserted at a line break, if

     - An indentation region can start at the current position in the source, and
     - the first token on the next line has an indentation width strictly greater
       than the current indentation width

    An indentation region can start

     - after the condition of an `if-else`, or
     - after the leading parameter(s) of a given extension method clause, or
     - after one of the following tokens:
    ```
    =  =>  <-  if  then  else  while  do  try  catch  finally  for  yield  match  return  with
    ```
    If an `<indent>` is inserted, the indentation width of the token on the next line
    is pushed onto `IW`, which makes it the new current indentation width.

 2. An `<outdent>` is inserted at a line break, if

    - the first token on the next line has an indentation width strictly less
        than the current indentation width, and
    - the first token on the next line is not a
        [leading infix operator](../changed-features/operators.html).

     If an `<outdent>` is inserted, the top element if popped from `IW`.
     If the indentation width of the token on the next line is still less than the new current indentation width, step (2) repeats. Therefore, several `<outdent>` tokens
     may be inserted in a row.

It is an error if the indentation width of the token following an `<outdent>` does not
match the indentation of some previous line in the enclosing indentation region. For instance, the following would be rejected.
```scala
if x < 0
    -x
  else   // error: `else` does not align correctly
     x
```
Indentation tokens are only inserted in regions where newline statement separators are also inferred:
at the toplevel, inside braces `{...}`, but not inside parentheses `(...)`, patterns or types.

### New Role of With

To make bracews optional for constructs like class bodies, the syntax of the language is changed so that a class body or similar construct may optionally be prefixed with `with`. Since `with` can start an indentation region, this means that all of the following syntaxes are allowed and are equivalent:
```scala
trait A {
  def f: Any
}
class C(x: Int) extends A {
  def f = x
}
type T = A {
  def f: Int
}
```
---
```scala
trait A with {
  def f: Int
}
class C(x: Int) extends A with {
  def f = x
}
type T = A with {
  def f: Int
}
```
---
```scala
trait A with
  def f: Int

class C(x: Int) extends A with
  def f = x

type T = A with
  def f: Int
```

The syntax changes allowing this are as follows:
```
TemplateBody ::=  [‘with’] ‘{’ [SelfType] TemplateStat {semi TemplateStat} ‘}’
EnumBody     ::=  [‘with’] ‘{’ [SelfType] EnumStat {semi EnumStat} ‘}’
Packaging    ::=  ‘package’ QualId [‘with’] ‘{’ TopStatSeq ‘}’
RefinedType  ::=  AnnotType {[‘with’] Refinement}
```
It is assumed here that braces following a `with` can be transparently replaced by an
indentation region.

With the new indentation rules, the previously allowed syntax
```
class A extends B with
                C
```
becomes illegal since `C` above would be terated as a nested statement inside `A`. More generally, a `with` that separates parent constructors cannot be at the end of a line. One has to write
```
class A extends B
           with C
```
instead (or replace the "`with`" by a "`,`"). When compiling in Scala-2 mode, a migration warning is issued for the illegal syntax and a (manual) rewrite is suggested.

### Spaces vs Tabs

Indentation prefixes can consist of spaces and/or tabs. Indentation widths are the indentation prefixes themselves, ordered by the string prefix relation. So, so for instance "2 tabs, followed by 4 spaces" is strictly less than "2 tabs, followed by 5 spaces", but "2 tabs, followed by 4 spaces" is incomparable to "6 tabs" or to "4 spaces, followed by 2 tabs". It is an error if the indentation width of some line is incomparable with the indentation width of the region that's current at that point. To avoid such errors, it is a good idea not to mix spaces and tabs in the same source file.

### Indentation and Braces

Indentation can be mixed freely with braces. For interpreting indentation inside braces, the following rules apply.

 1. The assumed indentation width of a multiline region enclosed in braces is the
    indentation width of the first token that starts a new line after the opening brace.

 2. On encountering a closing brace `}`, as many `<outdent>` tokens as necessary are
    inserted to close all open indentation regions inside the pair of braces.

### Special Treatment of Case Clauses

The indentation rules for `match` expressions and `catch` clauses are refined as follows:

 - An indentation region is opened after a `match` or `catch` also if the following `case`
   appears at the indentation width that's current for the `match` itself.
 - In that case, the indentation region closes at the first token at that
   same indentation width that is not a `case`, or at any token with a smaller
     indentation width, whichever comes first.

The rules allow to write `match` expressions where cases are not indented themselves, as in the example below:
```scala
x match
case 1 => print("I")
case 2 => print("II")
case 3 => print("III")
case 4 => print("IV")
case 5 => print("V")

println(".")
```

### The End Marker

Indentation-based syntax has many advantages over other conventions. But one possible problem is that it makes it hard to discern when a large indentation region ends, since there is no specific token that delineates the end. Braces are not much better since a brace by itself also contains no information about what region is closed.

To solve this problem, Scala 3 offers an optional `end` marker. Example
```scala
def largeMethod(...) =
    ...
    if ... then ...
    else
        ... // a large block
    end if
    ... // more code
end largeMethod
```
An `end` marker consists of the identifier `end` which follows an `<outdent>` token, and is in turn followed on the same line by exactly one other token, which is either an identifier or one of the reserved words
```scala
if  while  for  match  try  new
```
If `end` is followed by a reserved word, the compiler checks that the marker closes an indentation region belonging to a construct that starts with the reserved word. If it is followed by an identifier _id_, the compiler checks that the marker closes a definition
that defines _id_ or a package clause that refers to _id_.

`end` itself is a soft keyword. It is only treated as an `end` marker if it
occurs at the start of a line and is followed by an identifier or one of the reserved words above.

It is recommended that `end` markers are used for code where the extent of an indentation region is not immediately apparent "at a glance". Typically this is the case if an indentation region spans 20 lines or more.

### Example

Here is a (somewhat meta-circular) example of code using indentation. It provides a concrete representation of indentation widths as defined above together with efficient operations for constructing and comparing indentation widths.

```scala
enum IndentWidth with
  case Run(ch: Char, n: Int)
  case Conc(l: IndentWidth, r: Run)

  def <= (that: IndentWidth): Boolean = this match
    case Run(ch1, n1) =>
      that match
        case Run(ch2, n2) => n1 <= n2 && (ch1 == ch2 || n1 == 0)
        case Conc(l, r)   => this <= l
    case Conc(l1, r1) =>
      that match
        case Conc(l2, r2) => l1 == l2 && r1 <= r2
        case _            => false

  def < (that: IndentWidth): Boolean =
    this <= that && !(that <= this)

  override def toString: String = this match
    case Run(ch, n) =>
      val kind = ch match
        case ' '  => "space"
        case '\t' => "tab"
        case _    => s"'$ch'-character"
      val suffix = if n == 1 then "" else "s"
      s"$n $kind$suffix"
    case Conc(l, r) =>
      s"$l, $r"

object IndentWidth with
  private inline val MaxCached = 40

  private val spaces = IArray.tabulate(MaxCached + 1)(new Run(' ', _))
  private val tabs = IArray.tabulate(MaxCached + 1)(new Run('\t', _))

  def Run(ch: Char, n: Int): Run =
    if n <= MaxCached && ch == ' ' then
      spaces(n)
    else if n <= MaxCached && ch == '\t' then
      tabs(n)
    else
      new Run(ch, n)
  end Run

  val Zero = Run(' ', 0)
end IndentWidth
```

### Settings and Rewrites

Significant indentation is enabled by default. It can be turned off by giving any of the options `-noindent`, `old-syntax` and `language:Scala2`. If indentation is turned off, it is nevertheless checked that indentation conforms to the logical program structure as defined by braces. If that is not the case, the compiler issues a warning.

The Dotty compiler can rewrite source code to indented code and back.
When invoked with options `-rewrite -indent` it will rewrite braces to
indented regions where possible. When invoked with with options `-rewrite -noindent` it will rewrite in the reverse direction, inserting braces for indentation regions.
The `-indent` option only works on [new-style syntax](./control-syntax.html). So to go from old-style syntax to new-style indented code one has to invoke the compiler twice, first with options `-rewrite -new-syntax`, then again with options
`-rewrite-indent`. To go in the opposite direction, from indented code to old-style syntax, it's `-rewrite -noindent`, followed by `-rewrite -old-syntax`.

### Variant: Indentation Marker `:`

Generally, the possible indentation regions coincide with those regions where braces `{...}` are also legal, no matter whether the braces enclose an expression or a set of definitions. There is one exception, though: Arguments to function can be enclosed in braces but they cannot be simply indented instead. Making indentation always significant for function arguments would be too restrictive and fragile.

To allow such arguments to be written without braces, a variant of the indentation scheme is implemented under
option `-Yindent-colons`. This variant is more contentious and less stable than the rest of the significant indentation scheme. In this variant, a colon `:` at the end of a line is also one of the possible tokens that opens an indentation region. Examples:

```scala
times(10):
  println("ah")
  println("ha")
```
or
```scala
xs.map:
  x =>
      val y = x - 1
      y * y
```
Colons at the end of lines are their own token, distinct from normal `:`.
The Scala grammar is changed in this variant so that colons at end of lines are accepted at all points
where an opening brace enclosing a function argument is legal. Special provisions are taken so that method result types can still use a colon on the end of a line, followed by the actual type on the next.

