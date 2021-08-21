---
layout: multipage-overview
title: "Built-in blog"
partof: scala3-scaladoc
num: 5
previous-page: static-site
next-page: site-versioning
---

Scaladoc allows you to include a simple blog in your documentation. For now, it
provides only basic features. In the future, we plan to include more advanced
features like tagging or author pages.

Blog is treated a little differently than regular static sites. This article will help you set up your own blog.

## Proper directory setup

All your blogposts must be put under `blog/_posts` directory.


```
├── blog
│   ├── _posts
│   │   └── 2016-12-05-implicit-function-types.md
│   └── index.html
```

If you are using yaml [sidebar]({% link _overviews/scala3-scaladoc/static-site.md %}#sidebar) don't forget to place

```
sidebar:
    - title: Blog
```

somewhere inside the `yaml` tree representing the sidebar sections. Scaladoc will attach under that section all of your blogposts.

## Naming convention

All the blogpost filenames should start with date in numeric format matching `YYYY-MM-DD`.
Example name is `2015-10-23-dotty-compiler-bootstraps.md`.


