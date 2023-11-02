//> using options -Xfatal-warnings -deprecation

val foo = manifest[List[? <: Int]] // warn
val bar = optManifest[Array[? <: String]] // warn
// nopos-error: No warnings can be incurred under -Werror.