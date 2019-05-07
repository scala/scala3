# Comment parsing
## Files from dotty-doc requiring work
* util.traversing
  * ImplicitlyAddedFrom
* Comment
  * Span + error printing
  * Packages map
  * Figure out ShortHtml
* CommentParser
  * ParseWikiAtSymbol (useless?)
  * Span + error printing
* HTMLParsers
  * Packages map
  * Multi level list not working when ordered and unordered are combined
* WikiParser (just error reporting (span))
  * Span + error printing

# Other
* handle things such as $genericCanBuildFromInfo
* Main

# Representation
* All modifiers present?
* Define wiki or markdown syntax
* Beautify type parameters (+ variance + bound two direction)
* Beautify everything (parents, etc.)
* Make sense of the parent field
* SuperTypes
* Known subclasses
* Annotations (+ everywhere or only where it makes sense?)
* Type default
* Alias type
* Make sure no Entity is left
* Verify we have every entities-like
* Remove showCode whenever possible

* Fix color in showcode
* Methods inherited doc

# Output
* Output inner class/object if not inherited
* Source file link
* List Package content => def/val too?
* Link Reference
* Case class remove artefacts?
* Class inside class print parents
* Rework class output

# Report
* Goal Why we wanted to generated from tasty why to markdown
* General architecture, same intermediate interface as dotty doc
* intermediate representation md to xxxx
* Difficulties, problmes with dottydoc + md libraries