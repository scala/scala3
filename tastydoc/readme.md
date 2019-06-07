# Tastydoc

This tool produces Markdown documentation files for Scala projects using TASTy files.

## How to run
Call main with the following arguments to produce Markdown documentation files: (Omitting { and })
* **--syntax** {*wiki or markdown*} Syntax for parsing user documentation
* **--packagestolink** {*regex1 regex2 ...*} Regexes of packages or entities (example: `scala.collection.*`). Only the types with a path matching these regexes will produce links in the documentation files
* **--classpath** {*URI*} Extra classpath for input files
* **-i** {*file1 file2 ...*} TASTy files
* **-d** {*dir1 dir2 ...*} Directories to recursively find TASTy files