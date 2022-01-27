---
layout: doc-page
title: "Settings"
---

This chapter lists the configuration options that can be used when calling scaladoc. Some of the information shown here can be found by calling scaladoc with the `-help` flag.

## Parity with scaladoc for Scala 2

Scaladoc has been rewritten from scratch and some of the features turned out to be useless in the new context.
If you want to know what is current state of compatibility with scaladoc old flags, you can visit this issue for tracking [progress](https://github.com/lampepfl/dotty/issues/11907).

## Providing settings

Supply scaladoc settings as command-line arguments, e.g., `scaladoc -d output -project my-project target/scala-3.0.0-RC2/classes`. If called from sbt, update the value of `Compile / doc / scalacOptions` and `Compile / doc / target` respectively, e. g.

```
Compile / doc / target ++= Seq("-d", "output")
Compile / doc / scalacOptions ++= Seq("-project", "my-project")
```

## Overview of all available settings

##### -project
The name of the project. To provide compatibility with Scala2 aliases with `-doc-title`

##### -project-version
The current version of your project that appears in a top left corner. To provide compatibility with Scala2 aliases with `-doc-version`

##### -project-logo
The logo of your project that appears in a top left corner. To provide compatibility with Scala2 aliases with `-doc-logo`

##### -project-footer
The string message that appears in a footer section. To provide compatibility with Scala2 aliases with `-doc-footer`

##### -comment-syntax
The styling language used for parsing comments.
Currently we support two syntaxes: `markdown` or `wiki`
If setting is not present, scaladoc defaults `markdown`

##### -revision
Revision (branch or ref) used to build project project. Useful with sourcelinks to prevent them from pointing always to the newest master that is subject to changes.

##### -source-links
Source links provide a mapping between file in documentation and code repository.

Example source links is:
`-source-links:docs=github://lampepfl/dotty/master#docs`

Accepted formats:

\<sub-path>=\<source-link>
\<source-link>

where \<source-link> is one of following:
 - `github://<organization>/<repository>[/revision][#subpath]`
     will match https://github.com/$organization/$repository/\[blob|edit]/$revision\[/$subpath]/$filePath\[$lineNumber]
     when revision is not provided then requires revision to be specified as argument for scaladoc
 - `gitlab://<organization>/<repository>`
     will match https://gitlab.com/$organization/$repository/-/\[blob|edit]/$revision\[/$subpath]/$filePath\[$lineNumber]
     when revision is not provided then requires revision to be specified as argument for scaladoc
 - \<scaladoc-template>

\<scaladoc-template> is a format for `doc-source-url` parameter from old scaladoc.
NOTE: We only supports `€{FILE_PATH_EXT}`, `€{TPL_NAME}`, `€{FILE_EXT}`,
 €{FILE_PATH}, and €{FILE_LINE} patterns.


Template can defined only by subset of sources defined by path prefix represented by `<sub-path>`.
In such case paths used in templates will be relativized against `<sub-path>`



##### -external-mappings

Mapping between regexes matching classpath entries and external documentation.

Example external mapping is:
`-external-mappings:.*scala.*::scaladoc3::https://scala-lang.org/api/3.x/,.*java.*::javadoc::https://docs.oracle.com/javase/8/docs/api/`

A mapping is of the form '\<regex>::\[scaladoc3|scaladoc|javadoc]::\<path>'. You can supply several mappings, separated by commas, as shown in the example.

##### -social-links

Links to social sites. For example:

`-social-links:github::https://github.com/lampepfl/dotty,gitter::https://gitter.im/scala/scala,twitter::https://twitter.com/scala_lang`

Valid values are of the form: '\[github|twitter|gitter|discord]::link'. Scaladoc also supports 'custom::link::white_icon_name::black_icon_name'. In this case icons must be present in 'images/' directory.

##### -skip-by-id

Identifiers of packages or top-level classes to skip when generating documentation.

##### -skip-by-regex

Regexes that match fully qualified names of packages or top-level classes to skip when generating documentation.

##### -doc-root-content

The file from which the root package documentation should be imported.

##### -author

Adding authors in docstring with `@author Name Surname` by default won't be included in generated html documentation.
If you would like to label classes with authors explicitly, run scaladoc with this flag.

##### -groups

Group similar functions together (based on the @group annotation)

##### -private

Show all types and members. Unless specified, show only public and protected types and members.

##### -doc-canonical-base-url

A base URL to use as prefix and add `canonical` URLs to all pages. The canonical URL may be used by search engines to choose the URL that you want people to see in search results. If unset no canonical URLs are generated.

##### -siteroot

A directory containing static files from which to generate documentation. Default directory is `./docs`

##### -no-link-warnings

Suppress warnings for ambiguous or incorrect links in members’ lookup. Doesn't affect warnings for incorrect links of assets etc.

##### -versions-dictionary-url

A URL pointing to a JSON document containing a dictionary: `version label -> documentation location`.
The JSON file has single property `versions` that holds the dictionary associating the labels of specific versions of the documentation to the URLs pointing to their index.html
Useful for libraries that maintain different versions of their documentation.

Example JSON file:
```
{
  "versions": {
    "3.0.x": "https://dotty.epfl.ch/3.0.x/docs/index.html",
    "Nightly": "https://dotty.epfl.ch/docs/index.html"
  }
}
```

##### -snippet-compiler

Snippet compiler arguments provide a way to configure snippet type checking.

This setting accepts a list of arguments in the format:
args := arg{,arg}
arg := [path=]flag
where `path` is a prefix of the path to source files where snippets are located and `flag` is the mode in which snippets will be type checked.

If the path is not present, the argument will be used as the default for all unmatched paths.

Available flags:
compile - Enables snippet checking.
nocompile - Disables snippet checking.
fail - Enables snippet checking, asserts that snippet doesn't compile.

The fail flag comes in handy for snippets that present that some action would eventually fail during compilation, e. g. [Opaques page](../../reference/other-new-features/opaques.md)

Example usage:

`-snippet-compiler:my/path/nc=nocompile,my/path/f=fail,compile`

Which means:

all snippets in files under directory `my/path/nc` should not be compiled at all
all snippets in files under directory `my/path/f` should fail during compilation
all other snippets should compile successfully

##### -Ysnippet-compiler-debug

Setting this option makes snippet compiler print the snippet as it is compiled (after wrapping).

##### -Ydocument-synthetic-types

Include pages providing documentation for the intrinsic types (e. g. Any, Nothing) to the docs. The setting is useful only for stdlib because scaladoc for Scala 3 relies on TASTy files, but we cannot provide them for intrinsic types since they are embedded in the compiler.
All other users should not concern with this setting.
