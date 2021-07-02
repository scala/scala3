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

Example front-matter
```
---
title: My custom title
---
```

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
On each level you can have three different types of entries: `page`, `blog` or `subsection`.

`Page` is a leaf of the structure and accepts following attributes:
- `title` \[optional\] - title of the page
- `page` \[mandatory\] - path to the file that will represent the page, it can be either html of markdown file to be rendered, there is also possibility to pass the `directory` path. If so, the scaladoc will render the directory and all its content as if there were no `sidebar.yml` basing of its tree structure and index files.

The `page` property

`Subsection` accepts nested nodes, these can be either pages or subsection, which allow you to create tree-like navigation. The attributes are:
- `title` \[optional\] - title of the page
- `index` \[optional\] - path to the file that will represent the index file of the subsection, it can be either html of markdown file to be rendered
- `subsection` \[mandatory\] - nested nodes, can be either pages or subsections

The `Subsection` can omit `title` or `index`, however not specifying any of these properties disables you from giving the title of the section.

The `Blog` is a special node represented by simple entry `- title: Blog` with no other attirbutes. All your blogposts will be automatically linked under this section. You can read more about blog [here](blog.md).

```
├── blog
│   ├── _posts
│   │   └── 2016-12-05-implicit-function-types.md
│   └── index.html
├── index.html
└── sidebar.yml
```

## Hierarchy of title

There is a possibility to give custom title using `sidebar.yml`. The default strategy when choosing title for:

#### Page

1. `title` from the `front-matter` of the markdown/html file
2. `title` property from the `sidebar.yml` property
3. filename

#### Subsection

1. `title` from the `front-matter` of the markdown/html index file
2. `title` property from the `sidebar.yml` property
3. filename

Note that if you skip `index` file in your tree structure of you don't specify the `title` in the frontmatter, there will be given generic name `index`. The same applies when using `sidebar.yml` but not specifying `title` nor `index`, just a subsection. Again, generic `index` name will appear.


## Static resources

You can attach static resources (pdf, images) to your documentation by using two dedicated directories:
`resources` and `images`. When you upload your assests under any of these directories you can reference them in markdown
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
This is my blogpost. Here is the image ![](my_image.png) and here is my [pdf](my_file.pdf)
```
