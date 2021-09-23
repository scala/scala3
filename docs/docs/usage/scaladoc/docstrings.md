---
layout: multipage-overview
title: "Docstrings - specific Tags and Features"
partof: scala3-scaladoc
num: 2
previous-page: index
next-page: linking
---

This chapter describes how to correctly write docstrings and how to use all the available features of scaladoc.
Since many things are the same as in the old scaladoc, some parts are reused from this [article](https://docs.scala-lang.org/overviews/scaladoc/for-library-authors.html)


Scaladoc extends Markdown with additional features, such as linking
to API definitions. This can be used from within static documentation and blog
posts to provide blend-in content.


## Where to put docstrings

Scaladoc comments go before the items they pertain to in a special comment block that starts with a /** and ends with a */, like this:

```scala
/** Start the comment here
  * and use the left star followed by a
  * white space on every line.
  *
  * Even on empty paragraph-break lines.
  *
  * Note that the * on each line is aligned
  * with the second * in /** so that the
  * left margin is on the same column on the
  * first line and on subsequent ones.
  *
  * Close the comment with *\/
  *
  * If you use Scaladoc tags (@param, @group, etc.),
  * remember to put them at separate lines with nothing preceding.
  *
  * For example:
  *
  * Calculate the square of the given number
  *
  * @param d the Double to square
  * @return the result of squaring d
  */
 def square(d: Double): Double = d * d
```

In the example above, this Scaladoc comment is associated with the method square since it is right before it in the source code.

Scaladoc comments can go before fields, methods, classes, traits, objects.
For now, scaladoc doesn't support straightforward solution to document packages. There is a dedicated github
[issue](https://github.com/lampepfl/dotty/issues/11284), where you can check the current status of the problem.

For class primary constructors which in Scala coincide with the definition of the class itself, a @constructor tag is used to target a comment to be put on the primary constructor documentation rather than the class overview.

## Tags

Scaladoc uses `@<tagname>` tags to provide specific detail fields in the comments. These include:

### Class specific tags

- `@constructor` placed in the class comment will describe the primary constructor.

### Method specific tags

- `@return` detail the return value from a method (one per method).

### Method, Constructor and/or Class tags

- `@throws` what exceptions (if any) the method or constructor may throw.
- `@param` detail a value parameter for a method or constructor, provide one per parameter to the method/constructor.
- `@tparam` detail a type parameter for a method, constructor or class. Provide one per type parameter.

### Usage tags

- `@see` reference other sources of information like external document links or related entities in the documentation.
- `@note` add a note for pre or post conditions, or any other notable restrictions or expectations.
- `@example` for providing example code or related example documentation.


### Member grouping tags

These tags are well-suited to larger types or packages, with many members. They allow you to organize the Scaladoc page into distinct sections, with each one shown separately, in the order that you choose.

These tags are not enabled by default! You must pass the -groups flag to Scaladoc in order to turn them on. Typically the sbt for this will look something like:

```scala
Compile / doc / scalacOptions ++= Seq(
  "-groups"
)
```
Each section should have a single-word identifier that is used in all of these tags, shown as `group` below. By default, that identifier is shown as the title of that documentation section, but you can use @groupname to provide a longer title.

Typically, you should put @groupprio (and optionally @groupname and @groupdesc) in the Scaladoc for the package/trait/class/object itself, describing what all the groups are, and their order. Then put @group in the Scaladoc for each member, saying which group it is in.

Members that do not have a `@group` tag will be listed as “Ungrouped” in the resulting documentation.

- `@group <group>` - mark the entity as a member of the `<group>` group.
- `@groupname <group> <name>` - provide an optional name for the group. `<name>` is displayed as the group header before the group description.
- `@groupdesc <group> <description>` - add optional descriptive text to display under the group name. Supports multiline formatted text.
- `@groupprio <group> <priority>` - control the order of the group on the page. Defaults to 0. Ungrouped elements have an implicit priority of 1000. Use a value between 0 and 999 to set a relative position to other groups. Low values will appear before high values.

### Other tags

- `@author` provide author information for the following entity
- `@version` the version of the system or API that this entity is a part of.
- `@since` like `@version` but defines the system or API that this entity was first defined in.
- `@deprecated` marks the entity as deprecated, providing both the replacement implementation that should be used and the version/date at which this entity was deprecated.
- `@syntax <name>` let you change the parser for docstring. The default syntax is markdown, however you can change it using this directive. Currently available syntaxes are `markdown` or `wiki`, e. g. `@syntax wiki`

### Macros

- `@define <name> <definition>` allows use of $name in other Scaladoc comments within the same source file which will be expanded to the contents of `<definition>`.

If a comment is not provided for an entity at the current inheritance level, but is supplied for the overridden entity at a higher level in the inheritance hierarchy, the comment from the super-class will be used.

Likewise if `@param`, `@tparam`, `@return` and other entity tags are omitted but available from a superclass, those comments will be used.

### Explicit

For explicit comment inheritance, use the @inheritdoc tag.

### Markup

Scaladoc provides two syntax parsers: `markdown` (default) or `wikidoc`.
It is still possible to embed HTML tags in Scaladoc (like with Javadoc), but not necessary most of the time as markup may be used instead.

#### Markdown

Markdown uses [commonmark flavour](https://spec.commonmark.org/current/) with two custom extensions:
- `wikidoc` links for referencing convenience
- `wikidoc` codeblocks with curly braces syntax


#### Wikidoc

Wikidoc is syntax used for scala2 scaladoc. It is supported because of many existing source code, however it is **not** recommended to use it in new projects.
Wikisyntax can be toggled on with flag `-comment-syntax wiki` globally, or with `@syntax wiki` directive in docstring.

Some of the standard markup available:

```
`monospace`
''italic text''
'''bold text'''
__underline__
^superscript^
,,subscript,,
[[entity link]], e.g. [[scala.collection.Seq]]
[[https://external.link External Link]], e.g. [[https://scala-lang.org Scala Language Site]]
```

For more info about wiki links look at this [chapter](#linking-to-api)

Other formatting notes

- Paragraphs are started with one (or more) blank lines. `*` in the margin for the comment is valid (and should be included) but the line should be blank otherwise.
- Headings are defined with surrounding `=` characters, with more `=` denoting subheadings. E.g. `=Heading=`, `==Sub-Heading==`, etc.
- List blocks are a sequence of list items with the same style and level, with no interruptions from other block styles. Unordered lists can be bulleted using `-`, numbered lists can be denoted using `1.`, `i.`, `I.`, or `a.` for the various numbering styles. In both cases, you must have extra space in front, and more space makes a sub-level.

The markup for list blocks looks like:

```
/** Here is an unordered list:
  *
  *   - First item
  *   - Second item
  *     - Sub-item to the second
  *     - Another sub-item
  *   - Third item
  *
  * Here is an ordered list:
  *
  *   1. First numbered item
  *   1. Second numbered item
  *     i. Sub-item to the second
  *     i. Another sub-item
  *   1. Third item
  */
```

### General Notes for Writing Scaladoc Comments

Concise is nice! Get to the point quickly, people have limited time to spend on your documentation, use it wisely.
Omit unnecessary words. Prefer returns X rather than this method returns X, and does X,Y & Z rather than this method does X, Y and Z.
DRY - don’t repeat yourself. Resist duplicating the method description in the @return tag and other forms of repetitive commenting.

More details on writing Scaladoc

Further information on the formatting and style recommendations can be found in Scala-lang scaladoc style guide.

## Linking to API

Scaladoc allows linking to API documentation with Wiki-style links. Linking to
`scala.collection.immutable.List` is as simple as
`[[scala.collection.immutable.List]]`. For more information on the exact syntax, see [doc comment documentation]({% link _overviews/scala3-scaladoc/linking.md %}#definition-links).
