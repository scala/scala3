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

* method linking
* Markdown parser broken

# Other
* handle things such as $genericCanBuildFromInfo

# Representation
* Make sense of the parent field (+ parents?)
* Inherited declaration
* Annotations (+ everywhere or only where it makes sense?)
* Type default in def
* Alias type
* Beautify type parameters (+ variance + bound two direction)
* Linking to type with a def with the same name
* User doc for inherited method
* Known subclasses
* Fix companion
* Problem when link is a type inside an object (due to the $)

# Output
* Output inner class/object if not inherited
* Don't link scala lib
* Case classes are linked right now
* Case class remove artefacts?
* Class inside class print parents
* Doc for packages

# Report
* Goal Why we wanted to generated from tasty why to markdown
* General architecture, same intermediate interface as dotty doc
* intermediate representation md to xxxx
* Difficulties, problems with dottydoc + md libraries
* benchmark?
* Say everything is split into function

# To Ask
* Too much operations on String for path?