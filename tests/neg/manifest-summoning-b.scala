//> using options -Xfatal-warnings -deprecation

val foo = manifest[List[? <: Int]] // error
val bar = optManifest[Array[? <: String]] // error
