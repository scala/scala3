---
layout: doc-page
title: Working with the Backend
---

The compiler backend is based on a fork of the Scala 2.11 `GenBCode` backend and
lives at 

> https://github.com/lampepfl/scala/tree/sharing-backend

The dotty source tree contains a git submodule in the directory
[scala-backend](https://github.com/lampepfl/dotty/tree/master/scala-backend)
that points to this fork. We do not compile every file in this submodule,
instead we add the subset of files we need to the dotty-compiler project in the
sbt build.

The most important thing to know when working with git submodules is that
their content is not automatically updated when you do `git checkout` or `git
pull`, instead everytime you switch branch, you need to do:

``` shell
$ git submodule update --init
```

## Environment setup

1. Set the following git configuration options to make working with submodules easier, 
see the [Git Book](https://git-scm.com/book/en/v2/Git-Tools-Submodules)
for more information:
``` shell
$ git config --global diff.submodule log
$ git config --global status.submodulesummary 1
$ git config --global push.recurseSubmodules check
```

2. Fork scala from github

> https://github.com/lampepfl/scala 

(in the following commands, `dotty-staging/scala` is used as a placeholder for your own fork).

3. Add the new remote to track the upstream branch
```shell
$ cd scala-backend
$ git remote add scala-staging git@github.com:dotty-staging/scala.git
$ cd ..
```

## Workflow when changing to the backend

```shell
$ cd scala-backend
$ git checkout -b my-feature-branch

# Make some changes ...
$ git push -u scala-backend

# Open a PR against https://github.com/lampepfl/scala/tree/sharing-backend
$ cd ..
```

Once your PR has been merged into the backend, you'll need to make another PR
against dotty itself to update the backend, the following commands should be run
in the root dotty repository, not in the submodule:

``` shell
# The --remote option will update the submodule to the latest commit in the
# https://github.com/lampepfl/dotty/tree/master/scala-backend branch
git submodule update --init --remote

git commit -am "Update backend to include ..."
# Then push and make a PR against dotty as usual
```
