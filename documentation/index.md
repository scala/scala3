# Dotty-dokka

**Documentation tool for Scala 3**

We are using [TASTY](https://github.com/lampepfl/dotty/blob/master/tasty/src/dotty/tools/tasty/TastyFormat.scala) to generate documentation. We aim to has all known and loved feature from scaladoc as well as new feature such as :

- integrated documentation and API
- has option for basic pluggablity
- and much more

**Yes, this page was generated using dotty-dokka**

You can learn more from out [documentation](scala3doc/index.html).

## Getting started

For now the recommended way to try out our project would be:
 - Clone our [repository](https://github.com/Virtuslab/dotty-dokka)
 - Run `sbt main -n <name> -o <output> -t <tasty-files> -cp <classpath> -s { <sources> } ` where
   - `<name>`: name of module in generated documentation
   - `<output>`: location where documentation should be created
   - `<tasty-files>`: is list of dirs or jars that contains tasty files that should be documented
   - `<classpath>`: classpath that was used to generate tasty files 
   - `<sources>`: links to source files of module that are used to link symbols on pages to their source file. They need to be supplied in form:
      local_dir=remote_dir#line_suffix e.g. src/main/scala=https://github.com/lampepfl/scala3doc/tree/master/src/main/scala#L 
   
   We also support `-d <documentation>` argument to provide static documentation. You can find more about that feature [here](static-page.html).
