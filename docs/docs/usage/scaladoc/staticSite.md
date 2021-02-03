---
title: Static documentation
---

# {{ page.title}}

Scaladoc is able to generate static sites, known from [Jekyll](http://jekyllrb.com/) or [Docusaurus](https://docusaurus.io/).
Having a combined tool allows to provide interaction between static documentation and API, thus allowing the two to blend naturally.

Creating a site is just as simple as in Jekyll. The site root contains the
layout of the site and all files placed there will be either considered static,
or processed for template expansion.

The files that are considered for template expansion must end in `*.{html,md}`
and will from here on be referred to as "template files" or "templates".

A simple "hello world" site could look something like this:

```
├── docs
│   └── getting-started.md
└── index.html
```

This will give you a site with the following files in generated documentation:

```
index.html
docs/getting-started.html
```

Scaladoc can transform both files and directories (to organize your documentation into tree-like structure). By default directories has title based on file name and has empty content. There is an option to include `index.html` or `index.md` (not both) to provide both content and properties like title (see [Properties](#properties)).

## Properties

Scaladoc uses the [Liquid](https://shopify.github.io/liquid/) templating engine
and provides a number of custom filters and tags specific to Scala
documentation.

In Scaladoc, all templates can contain YAML front-matter. The front-matter
is parsed and put into the `page` variable available in templates via Liquid.

Scaladoc uses some predefined properties to controls some aspect of page.

Predefined properties:

 - **title** provide page title that will be used in navigation and html metadata.
 - **extraCss** additional `.css` files that will be included in this page. Paths should be relative to documentation root. **This setting is not exported to template engine.**
 - **extraJs** additional `.js` files that will be included in this page. Paths should be relative to documentation root. **This setting is not exported to template engine.**
 - **hasFrame** when set to `false` page will not include default layout (navigation, breadcrumbs etc.) but only token html wrapper to provide metadata and resources (js and css files). **This setting is not exported to template engine.**
- **layout** - predefined layout to use, see below. **This setting is not exported to template engine.**


## Using existing Templates and Layouts

To perform template expansion, Dottydoc looks at the `layout` field in the front-matter.
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

Sidebar
=======
Scaladoc by default uses layout of files in `docs` directory to create table of content. There is also ability to override it by providing a `sidebar.yml` file in the site root:

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
depth limit is 2 however it accepts both files and directories and latter can be used to provide deeper structures.

The items which have on the `subsection` level does not accepts `url`.

```
├── blog
│   └── _posts
│       └── 2016-12-05-implicit-function-types.md
├── index.html
└── sidebar.yml
```
