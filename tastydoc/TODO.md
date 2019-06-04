# Comment parsing
## Files from dotty-doc requiring work
* util.traversing
  * ImplicitlyAddedFrom
  * Fix linking for inherited method
* Comment
  * Span + error printing
  * Figure out ShortHtml
* CommentParser
  * Span + error printing
* WikiParser (just error reporting (span))
  * Span + error printing

* Check remaining HTML

# Other
* handle things such as $genericCanBuildFromInfo
* Check remaining TODOs/TOASKs/TOFIX
* Revert back to object name with $?

# Representation
* Make sense of the parents field
* Id for typeparams so we can link (Do not link)
* Inherited declaration
* Alias type
* Beautify type parameters (+ variance + bound two direction)
* Rework type matching
* Type like Graph.Node
* Remove artifcat and synthetic in package members
* Problem linking to objects like ContextualFunctionBetaReduction

# Output
* Output inner class/object if not inherited
* Doc for packages
* Check spacing in comment

# Report
* Be consistent in methods/function and parameters/arguments

# To Ask
* REFINED TYPES
* INLINE
* Error in calling owner
* Order in type matching symbol counts...

* Case classes are linked right now
* Abstract flag not working
* default value in def
* Not consistant reference (f.ex object in parents)