---
layout: doc-page
title: Basic Operations with Mill
---

Here's an example of how to test a project that uses mill:

```bash
mill utest.jvm[2.12.8].test
```

- `utest.jvm` - the name of the compiled module (obtain from `build.sc`)
- `2.12.8` – Scala cross-compile version
- `test` – task to run on the module specified with the specified Scala version

To get mill of the most recent version, first, find it in https://github.com/lihaoyi/mill/releases (e.g. `0.4.2-1-020e28`). Copy the download link and substitute it in the following command instead of `https://github.com/lihaoyi/mill/releases/download/0.4.1/0.4.1`:

```bash
# From http://www.lihaoyi.com/mill/
sudo sh -c '(echo "#!/usr/bin/env sh" && curl -L https://github.com/lihaoyi/mill/releases/download/0.4.1/0.4.1) > /usr/local/bin/mill && chmod +x /usr/local/bin/mill'
```
