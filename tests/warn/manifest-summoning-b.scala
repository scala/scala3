//> using options  -deprecation

val foo = manifest[List[? <: Int]] // warn
val bar = optManifest[Array[? <: String]] // warn