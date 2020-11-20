---
title: Scala3doc
---

![Scala3doc logo](/images/scala3doc-logo.png)

Scala3doc is tool to generate documentation for your Scala 3 projects. It provies similar features to `javadoc` or `scaladoc` as well as `jekyll` or `docusaurus`.

As you probably guessed, this whole site was created using scala3doc.


{% for post in site.posts %}
## [{{ post.title }}](/{{ post.url }})

{{ post.excerpt }}

[ (read more) ](/{{ post.url }})

{% endfor %}

## Other extensions

We would love to have your feedback on what you think would be good in order to
render the documentation you want! Perhaps you would like to render method
definitions or members? Do you want to have runnable code samples? Let us know by filing
[issues](https://github.com/lampepfl/dotty/issues/new)!