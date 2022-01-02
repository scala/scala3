# <span id="top">More about Pandoc</span> <span style="size:25%;"><a href="../README.md" title="Back to README">↩</a></span>
<!-- created by mics (https://github.com/michelou/) on December 2020 -->

The [Pandoc] software is free and available on Linux, MacOS and MS Windows.

The setup of [Pandoc] in the different environments is straightforward :
- *MS Windows*: we download archive [`pandoc-2.16.<X>-windows-x86_64.zip`][pandoc_downloads] and extract its contents into directory `C:\opt\pandoc-2.16.<X>`.
- *Ubuntu* : we download archive [`pandoc-2.16.<X>-linux-amd64.tar.gz`][pandoc_downloads], extract its contents into directory `/opt/pandoc-2.16.<X>/` and create a symbolic link `/opt/pandoc -> /opt/pandoc-2.16.<X>`.
- *Docker* : we use [`pandoc/ubuntu-latex`][docker_pandoc], a Ubuntu based image which bundles [TeX Live][tex_live] and [Pandoc] and is available from the [Docker Hub][docker_hub].
  > See document [`DOCKER.md`](./DOCKER.md) for further information, e.g. `Dockerfile` usage.


The [`md2pdf`](../md2pdf) script (resp. [`md2pdf.bat`](../md2pdf.bat)) executes the [Pandoc](https://pandoc.org/) command line tool with the desired options :

<table style="max-width:720px;">
<tr>
  <td><code>--data-dir="$DATA_DIR"</code></td>
  <td>User data directory, e.g. <a href="../data/"><code>data/</code></a>.</td>
</tr>
<tr>
  <td><code>--defaults="$DEFAULTS_FILE"</code></td>
  <td>Defaults file, e.g. <a href="../data/defaults.yaml"><code>data/defaults.yaml</code></a></td>
</tr>
<tr>
  <td><code>--syntax-definition="$YNTAX_FILE"</code></td>
  <td>Syntax file, e.g. <a href="../data/templates/scala.xml"><code>data/templates/scala.xml</code></a></td>
</tr>
<tr>
  <td><i><code>&lt;more_options&gt;</code></i> <sup id="anchor_01"><a href="#footnote_01">[1]</a></sup></td>
  <td>further options, like variable definitions</td>
</tr>
<tr>
  <td><code>--template="$TEMPLATE_FILE"</code></td>
  <td>Template file, e.g. <a href="../data/templates/template.tex"><code>data/templates/template.tex</code></a></td>
</tr>
<tr>
  <td><code>--pdf-engine="$LATEX_CMD"</code> <sup id="anchor_02"><a href="#footnote_02">[2]</a></sup></td>
  <td>LaTeX command, e.g. <a href="http://www.luatex.org/"><code>lualatex</code></a> or <code>xelatex</code></td>
</tr>
<tr>
  <td><code>--output="$OUTPUT_FILE"</code></td>
  <td>Output file, e.g. <code>target/scala3_reference.pdf</code></td>
</tr>
</table>

## <span id="footnotes">Footnotes</span>

<span id="footnote_01">[1]</span> ***Pandoc options*** [↩](#anchor_01)

<dl><dd>
Several Pandoc options can be specified either on the command line or in a defaults file (e.g. <a href="../data/defaults.yaml"><code>data/defaults.yaml</code></a>); changes to the current settings should be made very carefully.
</dd></dl>

<span id="footnote_02">[2]</span> ***PDF engines*** [↩](#anchor_02)

<dl><dd>
In this project we use either <a href="http://www.luatex.org/"><code>lualatex</code></a> or <code>xelatex</code> as PDF engines since they both support UTF-8 encoded Markdown documents, unlike <a href="https://linux.die.net/man/1/pdflatex"><code>pdflatex</code></a> (see also the article <a href="https://www.overleaf.com/learn/latex/Choosing%20a%20LaTeX%20Compiler">Choosing a LaTeX Compiler</a>).<br/>We successfully tested them in several environments, namely MS Windows 10, <a href="https://www.msys2.org/">MSYS2</a>, Ubuntu 18.04 (Bionic) and Docker (TeX Live based image).
</dd></dl>

***

*[mics](https://github.com/michelou/)/January 2022* [**&#9650;**](#top "Back to top")
<span id="bottom">&nbsp;</span>

[docker_entrypoint]: https://docs.docker.com/engine/reference/builder/#entrypoint "ENTRYPOINT instruction"
[docker_env]: https://docs.docker.com/engine/reference/builder/#env
[docker_hub]: https://hub.docker.com/
[docker_pandoc]: https://hub.docker.com/r/pandoc/ubuntu-latex
[pandoc]: https://pandoc.org/ "A universal document converter"
[pandoc_downloads]: https://github.com/jgm/pandoc/releases/
[tex_live]: https://tug.org/texlive/
