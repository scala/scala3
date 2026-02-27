# Contributing to Scala 3

Thanks for being willing to contribute!
We maintainers love contributions that fix bugs, improve error messages, simplify code, speed up the compiler,
and generally make the Scala 3 experience better.

Look at this repo's issues to find useful areas for contributions.
You can of course open your own if you've spotted a bug or have an idea for improvement.
We will generally not accept pull requests without a corresponding issue, unless they are extremely simple such as fixing typos.

Please understand that _we have limited time_ and _the low-hanging fruit in a compiler isn't hanging that low_.
If you're a Scala beginner, we recommend you start by contributing to other Scala projects that are less complex than the compiler.
Scaladex has [a list of Scala projects looking for contributions](https://index.scala-lang.org/search?q=&contributingSearch=true)

One good way to get started is to participate in the [Scala Compiler Spree](https://airtable.com/app94nwzow5R6W1O6/pagvjIzxYnqTTlhwY/form)!
You'll be assigned to a team and work with experienced contributors to fix an issue.

To learn how to set up your local environment, how the compiler works, how to reproduce issues, and so on,
head to the [Scala 3 Contributing Guide](https://nightly.scala-lang.org/docs/contributing/index.html).

Refer to our [LLM policy](LLM_POLICY.md) for rules and guidelines regarding usage of LLM-based tools in your contributions.

## Dos

- *Do* let us know you'd like to work on an issue by posting a comment, so we can keep track of work and avoid duplicate work
- *Do* discuss a sketch of your solution on an issue if it's nontrivial so we can give you feedback
- *Do* open a draft pull request if you have a solution that mostly works but need help on specific sub-problems
- *Do* ask questions if there are specific things you can't find in the documentation or in the code
- *Do* make sure you understand what the code you touch is doing and why your solution works
- *Do* keep in mind general software engineering practices, such as encapsulation, code sharing, and keeping code style consistent
- *Do* ping maintainers if you think you've done everything on your side and aren't getting a response after a few weeks

## Don'ts

- *Don't* open large pull requests with no prior discussion
- *Don't* make stylistic changes based on your personal taste that make pull requests harder to review
- *Don't* open pull requests you cannot explain even if they pass tests, such as "don't call this method on those inputs because it crashes"
  (it is easy to accidentally introduce unsoundness this way, and special cases are generally not the way to go)
- *Don't* try to fix `TODO`s or `FIXME`s in the codebase without a deep understanding of their context
  (remember, if they took 5 minutes to fix, the person writing them would most likely have fixed them instead of adding a comment)
- *Don't* ask general questions such as "how does a compiler work" or "what is JVM bytecode", these are better answered by existing resources
- *Don't* expect maintainers to respond immediately every time you have a question
- *Don't* break our LLM policy linked above, especially in the form of "vibe coded" PRs that you don't understand yourself

## Maintainers

Principal areas of the compiler and internal team members responsible for their maintenance:

**Compiler**
- Parser: @odersky
- Typer: @odersky, @noti0nal, @bracevac, (@smarter)
- Erasure: @odersky, (@smarter)
- Enums: @zielinsky
- Derivation & Mirrors:
- Export: @odersky
- Pattern Matching: @sjrd, @noti0na1, @zielinsky
- Inline: @odersky, @jchyb
- Metaprogramming (Quotes, Reflect, Staging): @jchyb
- Match types: @sjrd, @Linyxus
- GADT: @Linyxus
- Initialization checker: @liufengyun, @olhotak, @EnzeXing
- Transforms: @sjrd, @odersky, @smarter
- Tailrec: @sjrd, @mbovel
- JS backend: @sjrd
- JVM backend: @sjrd, @tanishiking, @lrytz
- Java-compat: @tanishiking, @lrytz, (@smarter)
- Extension Methods: @odersky
- Safe nulls (experimental): @noti0na1
- Capture checker (experimental): @odersky, @Linyxus, @bracevac, @noti0na1
- Modularity (experimental): (@KacperFKorban)
- Named Tuples: @odersky, @aherlihy
- Standard library: @lrytz, @SethTisue, @natsukagami, @noti0na1 

**Tooling**
- REPL:
- Runner/CLI: @Gedochao, (@tgodzik)
- IDE: @tgodzik, @zielinsky
- Scaladoc: (@Florian3k)
- SemanticDB: @natsukagami, (@tanishiking)
- Coverage: (@KacperFKorban)
- Linting (especially unused warnings) / Reporting UX: @som-snytt, (@tgodzik)
- Presentation Compiler: @tgodzik, @natsukagami, @zielinsky, (@rochala)
- Debug Adapter: @tgodzik
- Scastie: @warcholjakub, (@rochala)

**Infrastructure**
- CI: @WojciechMazur
- Community Build: @WojciechMazur
- Open Community Build: @WojciechMazur
- Vulpix:
- Benchmarks: @mbovel
- Releases: @WojciechMazur (Scala 3 Next), @tgodzik (Scala 3.3 LTS)
