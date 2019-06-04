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
* Check remaining TODOs/TOASKs/TOFIX

# Representation
* Make sense of the parents field
* Id for typeparams so we can link (Do not link)
* Alias type
* Rework type matching
* Remove artifcat and synthetic in package members

# Output
* Output inner class/object if not inherited
* Doc for packages
* Check spacing in comment
* Type like Graph.Node

# Features for beyond the project
* TypeLambdas
* Inherited declaration
* handle things such as $genericCanBuildFromInfo

# Report
* Be consistent in methods/function and parameters/arguments

# To Ask
* REFINED TYPES
* INLINE
* Error in calling owner
* Order in type matching symbol counts...
* Remove artifacts in packages

* Case classes are linked right now
* Abstract flag not working
* default value in def
* Not consistant reference (f.ex object in parents)