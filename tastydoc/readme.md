# Tastydoc

This tool produces Markdown documentation files for Scala projects using TASTy files.

## How to run
Call main with the following arguments to produce Markdown documentation files:
* **[--syntax SYNTAX]** Syntax for parsing user documentation (either *wiki or markdown*)
* **[--packagestolink REGEXES...]** Regexes of packages or entities (example: `scala.collection.*`). Only the types with a path matching these regexes will produce links in the documentation files
* **[--classpath URI]** Extra classpath for input files
* **[-i FILES...]** TASTy files
* **[-d DIRECTORIES...]** Directories to recursively find TASTy files