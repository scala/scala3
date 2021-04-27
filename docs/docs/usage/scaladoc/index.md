---
title: scaladoc
---

![scaladoc logo](/images/scaladoc-logo.png)

scaladoc is a tool to generate documentation for your Scala 3 projects. It provides similar features to `javadoc` as well as `jekyll` or `docusaurus`.

As you probably have guessed, this whole site was created using scaladoc.


{% for post in site.posts %}
## [{{ post.title }}](/{{ post.url }})

{{ post.excerpt }}

[ (read more) ](/{{ post.url }})

{% endfor %}

## Other extensions

We would love to have your feedback on what you think would be good in order to
render the documentation you want! Perhaps you would like to render method
definitions or members? Do you want to have runnable code snippets? Let us know
by filing [issues](https://github.com/lampepfl/dotty/issues/new)!
