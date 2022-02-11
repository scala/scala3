---
layout: doc-page
title: Dottydoc [Legacy]
---

Dottydoc is a tool to generate a combined documentation and API reference for
your project.

In previous versions of the Scaladoc tool, there is a big divide between what
is documentation and what is API reference. Dottydoc allows referencing, citing
and rendering parts of your API in your documentation, thus allowing the two to
blend naturally.

To do this, Dottydoc is very similar to what [Jekyll](http://jekyllrb.com/)
provides in form of static site generation. As you probably guessed, this
whole site was created using Dottydoc.

Creating a site is just as simple as in Jekyll. The site root contains the
layout of the site and all files placed here will be either considered static,
or processed for template expansion.

The files that are considered for template expansion must end in `*.{html,md}`
and will from here on be referred to as "template files" or "templates".

A simple "hello world" site could look something like this:

```
├── docs
│   └── getting-started.md
└── index.html
```

This will give you a site with the following endpoints:

```
_site/index.html
_site/docs/getting-started.html
```

Just as with Jekyll, the site is rendered in a `_site` directory.

Using existing Templates and Layouts
====================================
Dottydoc uses the [Liquid](https://shopify.github.io/liquid/) templating engine
and provides a number of custom filters and tags specific to Scala
documentation.

In Dottydoc, all templates can contain YAML front-matter. The front-matter
is parsed and put into the `page` variable available in templates via Liquid.

To perform template expansion, Dottydoc looks at `layout` in the front-matter.
Here's a simple example of the templating system in action, `index.html`:

```html
---
layout: main
---

<h1>Hello world!</h1>
```

With a simple main template like this:

{% raw %}
```html
<html>
    <head>
        <title>Hello, world!</title>
    </head>
    <body>
        {{ content }}
    </body>
</html>
```

Would result in `{{ content }}` being replaced by `<h1>Hello world!</h1>` from
the `index.html` file.
{% endraw %}

Layouts must be placed in a `_layouts` directory in the site root:

```
├── _layouts
│   └── main.html
├── docs
│   └── getting-started.md
└── index.html
```

It is also possible to use one of the [default layouts](#default-layouts) that ship with Dottydoc.

Blog
====
Dottydoc also allows for a simple blogging platform in the same vein as Jekyll.
Blog posts are placed within the `./_blog/_posts` directory and have to be in
the form `year-month-day-title.{md,html}`.

An example of this would be:

```
├── blog
│   └── _posts
│       └── 2016-12-05-implicit-function-types.md
└── index.html
```

To be rendered as templates, each blog post should have front-matter and a
`layout` declaration.

The posts are also available in the variable `site.posts` throughout the site.
The fields of these objects are the same as in
`[BlogPost](dotty.tools.dottydoc.staticsite.BlogPost)`.

Includes
========
In Liquid, there is a concept of include tags, these are used in templates to
include other de facto templates:

```html
<div class="container">
    {% raw %}{% include "sidebar.html" %}{% endraw %}
</div>
```

You can leave out the file extension if your include ends in `.html`.

Includes need to be kept in `_includes` in the site root. Dottydoc provides a
couple of [default includes](#default-includes), but the user-specified
includes may override these.

An example structure with an include file "sidebar.html":

```
├── _includes
│   └── sidebar.html
├── blog
│   ├── _posts
│   │   └── 2016-12-05-implicit-function-types.md
│   └── index.md
└── index.html
```

Sidebar
=======
Dottydoc gives you the ability to create your own custom table of contents,
this can either be achieved by overriding the `toc.html` or by
providing a `sidebar.yml` file in the site root:

```yaml
sidebar:
    - title: Blog
      url: blog/index.html
    - title: Docs
      url: docs/index.html
    - title: Usage
      subsection:
        - title: Dottydoc
          url: docs/usage/dottydoc.html
        - title: sbt-projects
          url: docs/usage/sbt-projects.html
```

The `sidebar` key is mandatory, as well as `title` for each element. The
default table of contents allows you to have subsections - albeit the current
depth limit is 2 -- we would love to see this changed, contributions welcome!

The items which have the `subsection` key, may not have a `url` key in the
current scheme. A site root example with this could be:

```
├── blog
│   └── _posts
│       └── 2016-12-05-implicit-function-types.md
├── index.html
└── sidebar.yml
```

Dottydoc Specific Tags and Behavior
====================================
Linking to API
--------------
If you for instance, want to link to `scala.collection.immutable.Seq` in a
markdown file, you can simply use the canonical path in your url:

```markdown
[Seq](scala.collection.immutable.Seq)
```

Linking to members is done in the same fashion:

```markdown
[Seq](scala.collection.immutable.Seq.isEmpty)
```

Dottydoc denotes objects by ending their names in "$". To select `List.range`
you'd therefore write:

```markdown
[List.range](scala.collection.immutable.List$.range)
```

Rendering Docstrings
--------------------
Sometimes you end up duplicating the docstring text in your documentation,
therefore Dottydoc makes it easy to render this inline:

```html
{% raw %}{% docstring "scala.collection.immutable.Seq" %}{% endraw %}
```

Other extensions
----------------
We would love to have your feedback on what you think would be good in order to
render the documentation you want! Perhaps you would like to render method
definitions or members? Let us know by filing
[issues](https://github.com/lampepfl/dotty/issues/new)!

Default Layouts
===============
main.html
---------
A wrapper for all other layouts, includes a default `<head>` with included
JavaScripts and CSS style-sheets.

### Variables ###
* `content`: placed in `<body>` tag
* `extraCSS`: a list of relative paths to extra CSS style-sheets for the site
* `extraJS`: a list of relative paths to extra JavaScripts for the site
* `title`: the `<title>` of the page

sidebar.html
------------
Sidebar uses `main.html` as its parent layout. It adds a sidebar generated from
a YAML file (if exists), as well as the index for the project API.

### Variables ###
* `content`: placed in a `<div>` with class `content-body`
* `docs`: the API docs generated from supplied source files, this is included by
  default and does not need to be specified.

doc-page.html
-------------
Doc page is used for pages that need a sidebar and provides a small wrapper for
the included {% raw %}`{{ content}}`{% endraw %}.

api-page.html
-------------
The last two layouts are special, in that they are treated specially by
Dottydoc. The input to the API page is a documented
`[Entity](dotty.tools.dottydoc.model.Entity)`. As such, this page can be changed
to alter the way Dottydoc renders API documentation.

blog-page.html
--------------
A blog page uses files placed in `./_blog/_posts/` as input to render a blog.

Default Includes
================
* `scala-logo.svg`: the scala in Dotty version as svg
* `toc.html`: the default table of contents template
