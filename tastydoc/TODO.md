# Comment parsing
## Files from dotty-doc requiring work
* util.traversing
  * ImplicitlyAddedFrom
* Comment
  * Span + error printing
  * Packages map
  * Figure out ShortHtml
* CommentParser
  * Span + error printing
* HTMLParsers
  * Packages map
* WikiParser (just error reporting (span))
  * Span + error printing

* Markdown parser broken
* linking method in userdoc

# Other
* handle things such as $genericCanBuildFromInfo
* Check remaining TODOs/TOASKs/TOFIX

# Representation
* Make sense of the parents field
* Id for typeparams so we can link
* Inherited declaration
* Alias type
* Beautify type parameters (+ variance + bound two direction)
* Rework type matching
* Type like Graph.Node
* Remove artifcat and synthetic in package members

# Output
* Output inner class/object if not inherited
* Doc for packages

# Report
* Goal Why we wanted to generated from tasty why to markdown
* General architecture, same intermediate interface as dotty doc
* intermediate representation md to xxxx
* Difficulties, problems with dottydoc + md libraries
* benchmark?
* Say everything is split into function
* Linking to type with a def with the same name
* Multi level list not working when ordered and unordered are combined (HTMLParsers)

# To Ask
* Too much operations on String for path?
* Case classes are linked right now
* Abstract flag not working
* default value in def