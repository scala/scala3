# Comment parsing
## Files from dotty-doc requiring work
* util.traversing
  * ImplicitlyAddedFrom
  * Fix linking for inherited method

# Other
* Check remaining TODOs/TOASKs/TOFIX

# Representation
* Id for typeparams so we can link (Do not link)
* Alias type
* Rework type matching

# Output
* Output inner class/object if not inherited

# To Ask
* Error in calling owner
* Order in type matching symbol counts...
* Remove artifacts in packages
* No way to get doc for packages?
* Abstract flag not working
* filter(!_.symbol.flags.is(Flags.StableRealizable)) good idea or not?