---
layout: doc-page
title: Significant Indentation
---

As an experimental feature, Scala 3 treats indentation as significant.

Indentation is significant everywhere except inside regions delineated by braces `{...}`, brackets `[...]` or parentheses `(...)` or within string or character literals.

Where indentation is significant, the compiler will insert `<indent>` or `<outdent>`
tokens at certain line breaks. Grammatically, pairs of `<indent>` and `<outdent>` tokens have the same effect as pairs of braces `{` and `}`.

The algorithm makes use of a stack `IW` of previously encountered indentation widths. The stack initially holds a single element with a zero indentation width. The _current indentation width_ is the indentation width of the top of the stack.

There are two rules:

 1. An `<indent>` is inserted at a line break, if

     - the first token on the next line has an indentation width strictly greater
	    than the current indentation width, and
     - the last token on the previous line can start an indentation region.

	 The following tokens can start an indentation region:
    ```
	  :  =  =>  <-  if  then  else  while  do  try  catch  finally  for  yield  match
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
if x < 0 then
    -x
  else   // error: `else` does not align correctly
	 x
```

Indentation prefixes can consist of spaces and tabs. Indentation widths are the indentation prefixes themselves, ordered by the string prefix relation. So, so for instance "2 tabs, followed by 4 spaces" is strictly less than "2 tabs, followed by 5 spaces", but "2 tabs, followed by 4 spaces" is incomparable to "6 tabs" or to "4 spaces, followed by 2 tabs". It is an error if the indentation width of some line is incomparable with the indentation width of the region that's current at that point. To avoid such errors, it is a good idea not to mix spaces and tabs in the same source file.

### Indentation Marker `:`

A colon `:` at the end of a line is one of the possible tokens that opens an indentation region. Examples:

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
The Scala grammar is changed so that colons at end of lines are accepted at all points
where an opening brace is legal, except if the previous token can already start an
indentation region. Special provisions are taken so that method result types can still use a colon on
the end of a line, followed by the actual type on the next.

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
If `end` is followed by a reserved word, the compiler checks that the marker closes an indentation region belonging to a construct that starts with the reserved word. If it is followed by an identifier _id_, the compiler checks that the marker closes an indentation region containing the right hand side of a `val`, `var`, or `def` or
the body of a class, trait, object, enum, given instance, or package clause that defines _id_.

`end` itself is a soft keyword. It is only treated as an `end` marker if it
occurs at the start of a line and is followed by an identifier or one of the reserved words above.

It is recommended that `end` markers are used for code where the extent of an indentation region is not immediately apparent "at a glance". Typically this is the case if an indentation region spans 20 lines or more.

### Example

Here is a (somewhat meta-circular) example of code using indentation. It provides a concrete representation of indentation widths as defined above together with efficient operations for constructing and comparing indentation widths.

```scala
enum IndentWidth:

    /** A run of `n` characters `ch` */
    case Run(ch: Char, n: Int)

    /** `l` followed by `r` */
    case Conc(l: IndentWidth, r: Run)

    def <= (that: IndentWidth): Boolean =
        this match
        case Run(ch1, n1) =>
            that match
            case Run(ch2, n2) => n1 <= n2 && (ch1 == ch2 || n1 == 0)
            case Conc(l, r)   => this <= l
        case Conc(l1, r1) =>
            that match
            case Conc(l2, r2) => l1 == l2 && r1 <= r2
            case _            => false

    def < (that: IndentWidth): Boolean = this <= that && !(that <= this)

    override def toString: String =
        this match
        case Run(ch, n) =>
            val kind = ch match
                case ' '  => "space"
                case '\t' => "tab"
                case _    => s"'$ch'-character"
            val suffix = if n == 1 then "" else "s"
            s"$n $kind$suffix"
        case Conc(l, r) =>
            s"$l, $r"

object IndentWidth:
    private inline val MaxCached = 40

    private val spaces = IArray.tabulate(MaxCached + 1):
        new Run(' ', _)
    private val tabs = IArray.tabulate(MaxCached + 1):
        new Run('\t', _)

    def Run(ch: Char, n: Int): Run =
        if n <= MaxCached && ch == ' ' then
            spaces(n)
        else if n <= MaxCached && ch == '\t' then
            tabs(n)
        else
            new Run(ch, n)

    val Zero = Run(' ', 0)
end IndentWidth
```

### Rewrites

The Dotty compiler can rewrite source code to indented code and back.
When invoked with options `-rewrite -indent` it will rewrite braces to
indented regions where possible. When invoked with with options `-rewrite -noindent` it will rewrite in the reverse direction, inserting braces for indentation regions.
The `-indent` option only works on [new-style syntax](./control-syntax.html). So to go from old-style syntax to new-style indented code one has to invoke the compiler twice, first with options `-rewrite -new-syntax`, then again with options
`-rewrite-indent`. To go in the opposite direction, from indented code to old-style syntax, it's `-rewrite -noindent`, followed by `-rewrite -old-syntax`.
