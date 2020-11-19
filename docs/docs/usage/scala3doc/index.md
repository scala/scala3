---
title: Scala3doc
---

![Scala3doc logo](/images/scala3doc-logo.png)

Scala3doc is a doctool dedicated to work with Scala 3.0. Beside basic features similar to `javadoc` or `scaladoc`. As you probably guessed, this
whole site was created using scala3doc.

{% for post in site.posts %}
## [{{ post.title }}](/{{ post.url }})

{{ post.excerpt }}

[ (read more) ](/{{ post.url }})

{% endfor %}

## Other extensions

We would love to have your feedback on what you think would be good in order to
render the documentation you want! Perhaps you would like to render method
definitions or members? Let us know by filing
[issues](https://github.com/lampepfl/dotty/issues/new)!