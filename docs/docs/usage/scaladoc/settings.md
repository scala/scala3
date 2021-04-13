---
title: Scaladoc settings
---

# {{page.title}}

You can find brief information included in this chapter while calling scaladoc with `-help` flag. However it can be a little tricky if one uses sbt `doc` task.

## Parity with scaladoc for Scala 2

Scaladoc has been rewritten from scratch and some of the features turned out to be useless in the new context.
If you want to know what is current state of compatibility with scaladoc old flags, you can visit this issue for tracking [progress](https://github.com/lampepfl/dotty/issues/11907).

## Providing settings

Scaladoc accepts settings provided as a string, e. g. `scaladoc -d output -project my-project target/scala-3.0.0-RC2/classes` or, if called from sbt,
update the value of `Compile / doc / scalacOptions`, e. g. `Compile / doc / scalacOptions ++= Seq("-d", "output", "-project", "my-project")`

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
The default syntax of comments. Defaults to markdown however wikisyntax can be used.

##### -revision
Revision (branch or ref) used to build project project. Useful with sourcelinks to prevent them from pointing always to the newest master that is subject to changes.

##### -source-links
Source links provide a mapping between file in documentation and code repository.

Accepted formats:
<\sub-path>=<\source-link>
<\source-link>

where <\source-link> is one of following:
 - `github://<organization>/<repository>[/revision][#subpath]`
     will match https://github.com/$organization/$repository/\[blob|edit]/$revision\[/$subpath]/$filePath\[$lineNumber]
     when revision is not provided then requires revision to be specified as argument for scaladoc
 - `gitlab://<organization>/<repository>`
     will match https://gitlab.com/$organization/$repository/-/\[blob|edit]/$revision\[/$subpath]/$filePath\[$lineNumber]
     when revision is not provided then requires revision to be specified as argument for scaladoc
 - <\scaladoc-template>

<scaladoc-template> is a format for `doc-source-url` parameter scaladoc.
NOTE: We only supports `€{FILE_PATH_EXT}`, `€{TPL_NAME}`, `€{FILE_EXT}`,
 €{FILE_PATH}, and €{FILE_LINE} patterns


Template can defined only by subset of sources defined by path prefix represented by `<sub-path>`.
In such case paths used in templates will be relativized against `<sub-path>`

Example source links is:
`-source-links:docs=github://lampepfl/dotty/master#docs`


##### -external-mappings

Mapping between regexes matching classpath entries and external documentation.
'regex::\[scaladoc|scaladoc|javadoc]::path' syntax is used

Example external mapping is:
`-external-mappings:.*scala.*::scaladoc3::http://dotty.epfl.ch/api/,.*java.*::javadoc::https://docs.oracle.com/javase/8/docs/api/`

##### -social-links

Links to social sites. '[github|twitter|gitter|discord]::link' syntax is used. 'custom::link::white_icon_name::black_icon_name' is also allowed, in this case icons must be present in 'images/'' directory.

Example social link is:

`-social-links:github::https://github.com/lampepfl/dotty,gitter::https://gitter.im/scala/scala,twitter::https://twitter.com/scala_lang`

##### -skip-by-id

Identifiers of packages or top-level classes to skip when generating documentation.

##### -skip-by-regex

Regexes that match fully qualified names of packages or top-level classes to skip when generating documentation.

##### -doc-root-content

The file from which the root package documentation should be imported.

##### -author

Include authors.

##### -groups

Group similar functions together (based on the @group annotation)

##### -private

Show all types and members. Unless specified, show only public and protected types and members.

##### -doc-canonical-base-url

A base URL to use as prefix and add `canonical` URLs to all pages. The canonical URL may be used by search engines to choose the URL that you want people to see in search results. If unset no canonical URLs are generated.

##### -siteroot

A directory containing static files from which to generate documentation. Default directory is `./docs`
