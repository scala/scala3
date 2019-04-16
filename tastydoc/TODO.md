# Comment parsing
## Files from dotty-doc requiring work
* util.traversing
  * ImplicitlyAddedFrom
* Comment
  * Span + error printing
  * Packages map
* CommentParser
  * ParseWikiAtSymbol (useless?)
  * Span + error printing
* HTMLParsers
  * Packages map
* WikiParser (just error reporting (span))
  * Span + error printing

## Representation
* Case class
* Trait
* Beautify type parameters (+ variance)
* Make type as References
  * Link them
  * Beautify them
* Beautify everything (parents, etc.)
* Make sense of the parent field
* SuperTypes
* Methods inherited ?
* Annotations (+ everywhere or only where it makes sense?")
* Verify we have everything

## Output
* Decide between html or markdown
* Actual output
* Different files for each class/object