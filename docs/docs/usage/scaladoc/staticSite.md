---
title: Static documentation
---

# {{ page.title }}

Scaladoc can generate static sites, known from [Jekyll](http://jekyllrb.com/) or [Docusaurus](https://docusaurus.io/).
Having a combined tool allows providing interaction between static documentation and API, thus allowing the two to blend naturally.

Creating a site is just as simple as in Jekyll. The site root contains the
the layout of the site and all files placed there will be either considered static,
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

Scaladoc can transform both files and directories (to organize your documentation into a tree-like structure). By default, directories have a title based on the file name and have empty content. It is possible to provide index pages for each section by creating `index.html` or `index.md` (not both) in the dedicated directory.

## Properties

Scaladoc uses the [Liquid](https://shopify.github.io/liquid/) templating engine
and provides several custom filters and tags specific to Scala
documentation.

In Scaladoc, all templates can contain YAML front-matter. The front-matter
is parsed and put into the `page` variable available in templates via Liquid.

Example front-matter
```
---
title: My custom title
---
```

Scaladoc uses some predefined properties to controls some aspects of page.

Predefined properties:

 - **title** provide page title that will be used in navigation and HTML metadata.
 - **extraCss** additional `.css` files that will be included in this page. Paths should be relative to the documentation root. **This setting is not exported to the template engine.**
 - **extraJs** additional `.js` files that will be included in this page. Paths should be relative to the documentation root. **This setting is not exported to the template engine.**
 - **hasFrame** when set to `false` page will not include default layout (navigation, breadcrumbs, etc.) but only token HTML wrapper to provide metadata and resources (js and css files). **This setting is not exported to the template engine.**
- **layout** - predefined layout to use, see below. **This setting is not exported to the template engine.**


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

## Sidebar

Scaladoc by default uses layout of files in `docs` directory to create table of content. There is also ability to override it by providing a `sidebar.yml` file in the site root:

```yaml
sidebar:
    - title: Blog
    - title: My title
      page: my-page1.md
    - page: my-page2.md
    - page: my-page3/subsection
    - title: Reference
      subsection:
        - page: my-page3.md
    - index: my-page4/index.md
      subsection:
        - page: my-page4/my-page4.md
    - title: My subsection
      index: my-page5/index.md
      subsection:
        - page: my-page5/my-page5.md
    - index: my-page6/index.md
      subsection:
        - index: my-page6/my-page6/index.md
          subsection:
            - page: my-page6/my-page6/my-page6.md
```

The `sidebar` key is mandatory.
On each level, you can have three different types of entries: `page`, `blog` or `subsection`.

`page` is a leaf of the structure and accepts the following attributes:
- `title` (optional) - title of the page
- `page` (mandatory) - path to the file that will represent the page, it can be either html or markdown file to be rendered, there is also the possibility to pass the `directory` path. If so, the scaladoc will render the directory and all its content as if there were no `sidebar.yml` basing on its tree structure and index files.

The `page` property

`subsection` accepts nested nodes, these can be either pages or subsections, which allow you to create tree-like navigation. The attributes are:
- `title` (optional) - title of the page
- `index` (optional) - path to the file that will represent the index file of the subsection, it can be either html or markdown file to be rendered
- `subsection` (mandatory) - nested nodes, can be either pages or subsections

In `subsection`s, you can omit `title` or `index`, however not specifying any of these properties prevents you from specifying the title of the section.

`blog` is a special node represented by simple entry `- title: Blog` with no other attributes. All your blog posts will be automatically linked under this section. You can read more about the blog [here](blog.md).

```
├── blog
│   ├── _posts
│   │   └── 2016-12-05-implicit-function-types.md
│   └── index.html
├── index.html
└── sidebar.yml
```

## Hierarchy of title

If the title is specified multiple times, the priority is as follows (from highest to lowest priority):

#### Page

1. `title` from the `front-matter` of the markdown/html file
2. `title` property from the `sidebar.yml` property
3. filename

#### Subsection

1. `title` from the `front-matter` of the markdown/html index file
2. `title` property from the `sidebar.yml` property
3. filename

Note that if you skip the `index` file in your tree structure or you don't specify the `title` in the frontmatter, there will be given a generic name `index`. The same applies when using `sidebar.yml` but not specifying `title` nor `index`, just a subsection. Again, a generic `index` name will appear.


## Static resources

You can attach static resources (pdf, images) to your documentation by using two dedicated directories:
`resources` and `images`. After placing your assets under any of these directories, you can reference them in markdown
as if they were relatively at the same level.

For example, consider the following situation:

```
├── blog
│   ├── _posts
│   │   └── 2016-12-05-implicit-function-types.md
│   └── index.html
├── index.html
├── resources
│   └── my_file.pdf
├── images
│   └── my_image.png
└── sidebar.yml

```

You can refer to the assets from within any of the files using markdown links:

```
This is my blog post. Here is the image ![](my_image.png) and here is my [pdf](my_file.pdf)
```
