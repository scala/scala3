---
layout: doc-page
title: "Spec Strings"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/spec-strings.html
---

Spec strings make agent prompts an integral, persistent part of source files. Each spec string expresses a programmer's intent for a piece of code.
Typically the code is then filled in by a coding agent. The difference to a prompt in a chat window is that a spec string stays an integral part of the program. So a developer can always see what spec generated a piece of code. What's more, the spec and the code are kept in sync for all future edits of the code base. If the spec changes, the agent will modify the code to match the changed spec. Conversely, if the developer changes some of the code, the agent's job is to modify the spec to describe the new code.

Spec strings are enabled under the `magic` experimental language import.
```
    import language.experimental.magic
```
`magic` is an acronym for **m**odular **ag**ent **i**nterface **c**ode. It enables an experimental version of Scala that is specifically targeted for interaction with coding agents. Spec strings are the first extension that falls under this import.

To work as intended, spec strings require tooling that invokes coding agents at certain points to achieve the synchronization between spec and code. The tooling could be part of a build tool, an IDE, or an agentic loop operating independently.

## Example

Say we want to write a function to parse dates. We define a `Date` type and give
a signature and spec string for the parsing function as follows:
```
type Date = (day: Int, month: Int, year: Int)

def parseDate(str: String): Option[Date] =
  '''spec
  Parse date string into a `Date` structure
  '''
  ???
```
Syntactically, a spec string is a variant of a [dedented string literal](https://github.com/scala/improvement-proposals/pull/112)
that is followed by `spec`
directly after the opening triple quotes. The string is always followed by some code (in our case, `???`).

There should be a command where the system is prompted to replace the `???` with an actual date parsing implementation. This could be automatic on a build-on-save or there could be a separate command for this action. When prompted, the system might change the program to:
```
def parseDate(str: String): Option[Date] =
  '''spec
  Parse date string into a `Date` structure
  '''
  str.split("-") match
    case Array(y, m, d) =>
      for
        year  <- y.toIntOption
        month <- m.toIntOption
        day   <- d.toIntOption
        if month >= 1 && month <= 12
        if day >= 1 && day <= 31
      yield Date(day, month, year)
    case _ => None
```
Maybe the programmer realizes that this is not the intended format and changes the prompt string to
```
def parseDate(str: String): Option[Date] =
  '''spec
  Parse date string in the format "day/month/year" into a `Date` structure
  '''
  str.split("-") match
    ...
```
On the next build-on-save the system will notice that the spec string has changed and will reconcile it
with the existing code by changing the code to match it again. It might thus complete the program to
```
def parseDate(str: String): Option[Date] =
  '''spec
  Parse date string in the format "day/month/year"
  into a `Date` structure.
  '''
  str.split("/") match
    case Array(d, m, y) =>
      for
        day   <- d.toIntOption
        month <- m.toIntOption
        year  <- y.toIntOption
        if month >= 1 && month <= 12
        if day >= 1 && day <= 31
      yield Date(day, month, year)
    case _ => None
```
Alternatively, the programmer could also have edited the previously generated code
to be the new version above. In this case the system would edit the spec string to correspond to the new code. The end result would be the same (modulo LLM non-determinism).

The response to a spec (change) can also involve system-generated code that does not follow directly the spec string. For instance, the programmer might choose yet another format by editing the spec string to:
```
  '''spec
  Parse date string into a `Date` structure. The string is in the
  format "day month year" where "month" is a three letter case-
  insensitive abbreviation of the month (e.g. "jan", "Feb").
  '''
```
The system might complete this to:
```
private val monthNames = Array("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

def parseDate(str: String): Option[Date] =
  '''spec
  Parse date string into a `Date` structure. The string is in the format "day month year"
  where "month" is a three letter case-insensitive abbreviation of the month (e.g. "jan", "Feb").
  '''
  str.split(" ") match
    case Array(d, m, y) =>
      for
        day <- d.toIntOption
        month = monthNames.indexWhere(_.equalsIgnoreCase(m)) + 1
        if month >= 1
        year <- y.toIntOption
        if day >= 1 && day <= 31
      yield Date(day, month, year)
    case _ => None
```
Here, the system added the definition of `monthNames` in addition to completing the body of `parseDate`.

## More Details

Spec strings are formatted in markdown syntax. The result of parsing and typing a spec string is an interpolated string literal. Like other interpolated literals, this is represented internally by a selection identifier (`$spec` in this case) on a string context that represents a string with holes and typed argument expressions that go in the holes.

The backtick code tags in markdown trigger "soft" string interpolations. That is, the compiler will try to parse, resolve and type-check the identifiers, expressions, or types in backticks and, if successful, turn them into interpolated expressions embedded in the string. IDEs should render well-typed content differently from ill-typed content. Well-typed content supports full navigation and editing actions including hyperlinking and global renaming.
Ill-typed content still lets the program compile, but hover on such content could reveal diagnostics such as "not found" or "type mismatch" as warnings.

For instance, we could have defined a format for dates somewhere under the name `formatSchema`. Then the following spec string could refer to it like this:
```
def parseDate(str: String): Option[Date] =
  '''spec
  Parse date string into a `Date` structure. The string is in the format given by `formatSchema`.
  '''
```
Internally, this will be represented as the following string context expression:
```
scala.compiletime.$spec:
  StringContext(
    "Parse date string into a `",
    "` structure. The string is in the format given by `",
    "`.")($wrappedType[Date], formatSchema)
```
Note that the two pieces of code in backticks, `Date` and `formatSchema`,
are extracted in the second argument list of the `StringContext`. This has several desirable consequences:

 - The IDE can use regular syntax highlighting for these pieces of code.
 - One can use IDE commands such as go-to-definition and show-documentation.
 - Global renamings of, say, `formatSchema` to `FormatTemplate` will
   also work on embedded code in spec strings.

The principle is that embedded code blocks in spec strings are treated as regular code, not comments.

The details of treating embedded code blocks are as follows: The compiler will first parse and type-check an embedded code block as an expression. If that succeeds, the expression becomes an interpolated part of the string.
Otherwise, the compiler will parse and type-check the code block as a type.
If that succeeds, the type is wrapped in the internal method `scala.compiletime.$wrappedType` and that expression becomes an interpolated part of the string. If neither attempt succeeds, the code block stays as it is,
but the reasons for the failure can be made available as warnings.

This scheme might find a companion object in the first pass when the intention is to refer to the corresponding class instead. This is no big deal since the compiler's data structures allow to navigate from a companion object to its companion class, so an agent harness using this info can easily adapt.

Spec strings also allow regular string interpolation with `$`'s. The difference is regular string interpolation must yield well-typed expressions, or the program won't compile.

The `compiletime.$spec` wrapper is an inline function that expands to `()`. Hence, spec strings are erased in the generated code; they have no runtime meaning.

## Implementation

We assume a coding agent that follows and changes the current editing state of the program. When invoked by the user, the coding agent will reconcile spec strings with the code they generate. To do that, the coding agent needs to know in what ways the program changed since its last invocation.

 - Have spec strings been added? This means new code has to be produced.
 - Have spec strings been changed? This means the generated code might also have to be changed, taking into account the previous code, and the diff between old and new spec strings.
 - Have definitions referenced by spec strings changed? This might also trigger changes in the associated code.
 - Has code generated by a spec string been changed manually by the user? This might require changing the associated spec string to keep the two in sync.

Most of these actions can be obtained by defining appropriate agent workflows. The workflows should have access to an MCP server that provides dependency information and a data store that can produce diffs between different versions of the source code. If every agent invocation coincides with a git commit, we could use git history for that, but this might be too coarse. The user might well want agentic completions of the code before doing a git commit. In that case, a separate store will be needed.

**IDE Requirements** The main requirement for an IDE is to render spec strings correctly: Do markdown rendering with special handling of code inside backticks. The IDE has the original source code and the compiler-generated interpolation expression with the semantic info. This is in principle similar to normal interpolated string rendering.

<!--
## Possible Generalization

We might want to generalize spec strings to '''-literals where the opening quote is followed by some arbitrary tag. Compiler plugins could then register to be invoked if a string with a given tag appears in the program. A compiler plugin may be an agent that has edit rights to the program. That could be the basis of the mechanics for making spec strings work the way it's postulated here.

There's a duality with syntax highlighting in markdown. Three backticks in markdown open a code block that is highlighted by the processor for the language tag. Three forward ticks in code open a markdown block that is processed by the tool registered for the string tag.
-->

## Summary

A spec string is a variant of a triple-quote string literal followed by statements and/or an expression. It has special meaning as a persistent way to specify a programmer's intent in an agent-augmented setting. Using coding agents, the system will make sure that a spec string corresponds to the code that follows it, up to the end of the block or to the next spec string, whichever comes first. This might also involve generation of helper definitions defined elsewhere.

Syntactically, spec strings generalize existing dedented string literal syntax and re-use existing code which would not make sense normally -- a string literal followed by some code is a no-op. We also re-use the existing meaning of Scala's `???` to mean "implement me", only now it's the agent who does that instead of the human. Internally, spec strings expand to interpolated string expressions which provide the entry points to a rich set of compiler-maintained relations that can be used by the coding agent.

## Discussion

It might seem more natural at first to see a spec string as some kind of doc-comment, which would mean that specs would be expressed in comments instead of the form presented here. But there are important differences between spec strings and comments.

 - Comments describe (or comment on) code. Spec strings generate code.
 - Doc-comments are placed in front of the methods they describe. Spec strings come
   after the signature and in front of the code that should follow. So a spec string
   is exactly at the point where the code it generates should go. Also, in that
   position spec strings can naturally refer to parameters of the preceding method signature.
 - As compile-time values, spec strings provide the necessary machinery for navigation, renaming and other editing actions. This helps in understanding what they mean and in keeping the program consistent under refactorings.
 - Spec strings are a form of quotation. They lift their contents from code to a spec that generates code. Note the duality with markdown syntax blocks, which are also a form of quotation. Three backquotes followed by "scala" opens a Scala code block inside markdown text whereas three forward quotes followed by "spec" opens a spec block formatted in markdown inside Scala code. This is recursive: Another triple backquote inside a spec block brings us back to Scala code and so on.

This is not to say that spec strings are not useful for documentation. They contain certainly highly relevant pieces of information. Maybe a document generator such as scaladoc could integrate them in its output.
