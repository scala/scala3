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

# Other
* Check remaining TODOs/TOASKs/TOFIX

# Representation
* Id for typeparams so we can link (Do not link)
* Alias type
* Rework type matching

# Output
* Output inner class/object if not inherited

# Features for beyond the project
* TypeLambdas
* Inherited declaration
* handle things such as $genericCanBuildFromInfo
* Type like Graph.Node
* default value in def
* Case classes are linked
* Error printing in parsing

# Report
* Be consistent in methods/function and parameters/arguments

# To Ask
* Error in calling owner
* Order in type matching symbol counts...
* Remove artifacts in packages
* No way to get doc for packages?
* Abstract flag not working
* filter(!_.symbol.flags.is(Flags.StableRealizable)) good idea or not?

* Not consistant reference (f.ex object in parents)