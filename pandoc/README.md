# <span id="top">Creating PDF files for the Scala 3 documentation</span>
<!-- created by mics (https://github.com/michelou/) on December 2020 -->

This project is located at the root of the [`lampepfl/dotty`][github_dotty] repository, at the same level as the [`Scaladoc`][github_scala3doc] project.

We aim to generate a *single* PDF file from a collection of [Markdown] documents, for instance for the [*Scala 3 reference*][scala3_reference] documentation.

Our solution is built on [Pandoc]; it gets its inputs from two directories and generates a PDF file into the output directory:

<table style="margin-top:-10px; max-width:650px;">
<tr>
<td><b>Input directories</b></td>
<td><code>docs/docs/&lt;project&gt;/</code><br/><code>pandoc/data/</code></td>
<td><code>*.md</code> files<br/><a href="https://pandoc.org/">Pandoc</a> data files</td>
</tr>
<tr>
<td><b>Output directory</b></td>
<td><code>out/pandoc/</code></td>
<td><code>scala3_<i>&lt;project&gt;</i>.pdf</code><sup>(*)</sup></td>
</tr>
</table>

<span style="margin-left:10px;font-size:90%;"><sup>(*)</sup> *`<project>`* is one of [`contributing`](https://github.com/lampepfl/dotty/tree/master/docs/docs/contributing), [`internals`](https://github.com/lampepfl/dotty/tree/master/docs/docs/internals), [`reference`](https://github.com/lampepfl/dotty/tree/master/docs/docs/reference) or [`usage`](https://github.com/lampepfl/dotty/tree/master/docs/docs/usage).</span>

The generated PDF file is more elaborated than its sibling HTML version; unlike the online [*Scala 3 reference*][scala3_reference] documentation available on the [Scala 3 documentation](https://docs.scala-lang.org/scala3/) webpage, the `scala3_reference.pdf` document :
- starts with a *title page* directly followed by a *table of contents* and ends with an *appendix*.
- gathers all the "*More details*" sections in the appendix.

> See document [`PROJECT.md`](docs/PROJECT.md) for further information, e.g. project organisation.

## <span id="dependencies">Project dependencies</span>

This project depends on the following software :

- [Pandoc 2](https://github.com/jgm/pandoc/releases) <sup id="anchor_01">[1](#footnote_01)</sup> *([release notes](https://pandoc.org/releases.html))*
- [TeX Live 2021](https://tug.org/texlive/) <sup id="anchor_02">[2](#footnote_02)</sup> *([release notes](http://www.tug.org/texlive/doc/texlive-en/texlive-en.html#x1-880009.2))*

> See documents [`PANDOC.md`](docs/PANDOC.md) and [`TEXLIVE.md`](docs/TEXLIVE.md) for product specific information, e.g. software installation.

One may also install the following software:

- [Docker Desktop 3](https://docs.docker.com/get-docker/) <sup id="anchor_03">[3](#footnote_03)</sup> *([release notes](https://docs.docker.com/release-notes/))*

> See document [`DOCKER.md`](docs/DOCKER.md) for further information, e.g. `Dockerfile` usage.

## <span id="commands">Build commands</span>

We provide two commands for MacOS/Ubuntu as well as their equivalents for MS Windows :
- for *interactive users* :<br/>[`build {<option>|<subcommand>}`](./build) (resp. [`build.bat`](./build.bat) on MS Windows)<br/><span style="font-size:80%;">(option `-help` displays the available options/subcommands).</span>
- for *automated tasks* (primarily, but not exclusively) :<br/>[`md2pdf [<project>]`](./md2pdf) (resp. [`md2pdf.bat`](./md2pdf.bat) on MS Windows)<br/><span style="font-size:80%;">(default value for `<project>` : `reference`).</span>

We also provide support for cloud development/deployment :

- with *Docker*<br/><a href="./Dockerfile"><code>Dockerfile</code></a>, a text document to create a Docker image featuring <a href="https://tug.org/texlive/">TeX Live</a>, <a href="https://pandoc.org/">Pandoc</a> and <a href="./md2pdf"><code>md2pdf</code></a>.
- with *GitHub Actions* <b style="color:red;">WIP</b> :<br/><a href="../.github/workflows/pandoc.yaml"><code>pandoc.yaml</code></a>, a workflow file to run CI jobs with <a href="https://docs.github.com/en/actions">GitHub Actions</a>.

> See document [`EXAMPLES.md`](docs/EXAMPLES.md) for usage examples in different environments.

## <span id="footnotes">Footnotes</span>

<span id="footnote_01">[1]</span> ***Pandoc software*** [↩](#anchor_01)

<dl><dd>
<a href="https://pandoc.org/">Pandoc</a> is both a library and a command line tool for converting files from one markup format into another. Through the command line option <a href="https://pandoc.org/MANUAL.html#option--pdf-engine"><code>--pdf-engine=&lt;path&gt;</code></a> Pandoc supports many PDF engines, among others <a href="https://linux.die.net/man/1/pdflatex"><code>pdflatex</code></a>, <a href="http://www.luatex.org/"><code>lualatex</code></a> and <code>xelatex</code>.
</dd></dl>

<span id="footnote_02">[2]</span> ***MiKTeX software*** [↩](#anchor_02)

<dl><dd>
As an alternative to <a href="https://tug.org/texlive/">TeX Live 2020</a> one may also use the <a href="https://miktex.org/">MiKTeX</a> distribution <i>(<a href="https://miktex.org/announcement/miktex-20-12">release notes</a>)</i> which we have tested on both MS Windows 10 and <a href="https://www.msys2.org/">MSYS2</a>.
</dd></dl>

<span id="footnote_03">[3]</span> ***Docker software*** [↩](#anchor_03)

<dl><dd>
We run <a href="https://docs.docker.com/docker-for-windows/install/">Docker Desktop for Windows</a> on our development machine.<br/>
Note that WSL 2 is required to interact with Docker on Windows from <a href="https://ubuntu.com/wsl">Ubuntu for WSL</a> (see documentation <a href="https://docs.docker.com/docker-for-windows/wsl/">Docker Desktop WSL 2 backend</a>).
</dd></dl>

***

*[mics](https://github.com/michelou/)/January 2022* [**&#9650;**](#top "Back to top")
<span id="bottom">&nbsp;</span>

[github_dotty]: https://github.com/lampepfl/dotty/#dotty
[github_scala3doc]: https://github.com/lampepfl/dotty/tree/master/scala3doc#scala3doc
[markdown]: https://commonmark.org/
[pandoc]: https://pandoc.org/ "A universal document converter"
[scala3_reference]: https://dotty.epfl.ch/docs/reference/overview.html
[tex_live]: https://tug.org/texlive/
