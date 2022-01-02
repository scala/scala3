# <span id="top">More about the project</span> <span style="size:25%;"><a href="../README.md" title="Back to README">↩</a></span>
<!-- created by mics (https://github.com/michelou/) on December 2020 -->

The `pandoc/` directory is organized as follows:

<table>

</tr>
<tr>
  <td><a href="../build"><code>build</code></a></td>
  <td><a href="https://linuxconfig.org/bash-scripting-tutorial">Bash script</a> for MacOS and Unix-like environments (e.g. <a href="https://ubuntu.com/">Ubuntu</a>, <a href="https://www.cygwin.com/">Cygwin</a>, <a href="https://www.msys2.org/">MSYS2</a>).<br/>
  <a href="../build"><code>build</code></a> provides user-friendly facilities (e.g. removal of output directory) besides calling <a href="../md2pdf"><code>md2pdf</code></a> (see also <a href="EXAMPLES.md"><code>EXAMPLES.md</code></a>).</td>
</tr>
<tr>
  <td><a href="../build.bat"><code>build.bat</code></a></td>
  <td><a href="https://en.wikibooks.org/wiki/Windows_Batch_Scripting">Batch file</a> for the MS Windows environment.<br/>
  <a href="../build.bat"><code>build.bat</code></a> provides user-friendly facilities (e.g. removal of the output directory) besides calling <a href="../md2pdf.bat"><code>md2pdf.bat</code></a> (see also <a href="EXAMPLES.md"><code>EXAMPLES.md</code></a>).</td>
</tr>
<tr>
  <td><a href="../md2pdf"><code>md2pdf</code></a></td>
  <td><a href="https://linuxconfig.org/bash-scripting-tutorial">Bash script</a> for Unix automated tasks (e.g. <a href="https://linuxconfig.org/using-cron-scheduler-on-linux-systems">cron</a>, <a href="https://docs.github.com/en/free-pro-team@latest/actions/learn-github-actions/introduction-to-github-actions">GitHub Actions</a>).<br/>
  <a href="../md2pdf"><code>md2pdf</code></a> executes the <a href="https://pandoc.org/MANUAL.html">Pandoc</a> command line tool with the appropriate settings.</td>
</tr>
<tr>
  <td><a href="../md2pdf.bat"><code>md2pdf.bat</code></a></td>
  <td><a href="https://en.wikibooks.org/wiki/Windows_Batch_Scripting">Batch file</a> for MS Windows automated tasks (e.g. <a href="https://docs.microsoft.com/en-us/windows/win32/taskschd/task-scheduler-start-page">Task Scheduler</a>, <a href="https://docs.github.com/en/free-pro-team@latest/actions/learn-github-actions/introduction-to-github-actions">GitHub Actions</a>).<br/><a href="../md2pdf.bat"><code>md2pdf.bat</code></a> executes the <a href="https://pandoc.org/MANUAL.html">Pandoc</a> command line tool with the appropriate settings.</td>
</tr>
<tr>
<td><a href="../.dockerignore"><code>.dockerignore</code></a></td>
<td>Directories/files to be ignored in the root directory of the Docker context (see also section <a href="https://docs.docker.com/engine/reference/builder/#dockerignore-file">"<code>.dockerignore</code> file"</a> in the Docker documentation).</td>
</tr>
<tr>
<td><a href="../Dockerfile"><code>Dockerfile</code></a></td>
<td>Text file to assemble an image for running <a href="../md2pdf"><code>md2pdf</code></a> on Docker.</td>
</tr>
<tr style="background-color:#eeeeee;">
  <td><i><code>data/</code></i></td>
  <td>contains master Markdown files for projects <a href="https://github.com/lampepfl/dotty/tree/master/docs/docs/internals"><code>internals</code></a>, <a href="https://github.com/lampepfl/dotty/tree/master/docs/docs/reference"><code>reference</code></a> and <a href="https://github.com/lampepfl/dotty/tree/master/docs/docs/usage"><code>usage</code></a>.</td>
</tr>
<tr>
  <td>&emsp;<a href="../data/defaults.yaml"><code>defaults.yaml</code></a></td>
  <td>YAML file defining <a href="https://pandoc.org/MANUAL.html#default-files">default Pandoc options</a>, including filters, e.g. <a href="../data/filters/include-files.lua"><code>include-file.lua</code></a>.</td>
</tr>
<tr>
  <td>&emsp;<a href="../data/internals.md"><code>internals.md</code></a></td>
  <td>Markdown file defining structure of the <a href="https://dotty.epfl.ch/docs/internals/backend.html"><i>Scala 3 Internals</i></a> documentation.</td>
</tr>
<tr>
  <td>&emsp;<a href="../data/reference.md"><code>reference.md</code></a></td>
  <td>Markdown file defining the structure of the <a href="https://dotty.epfl.ch/docs/reference/overview.html"><i>Scala 3 Reference</i></a> documentation.</td>
</tr>
<tr>
  <td>&emsp;<a href="../data/usage.md"><code>usage.md</code></a></td>
  <td>Markdown file defining the structure of the <a href="https://dotty.epfl.ch/docs/usage/getting-started.html"><i>Scala 3 Usage</i></a> documentation.</td>
</tr>
<tr style="background-color:#eeeeee;">
  <td><i><code>data/filters/</code></i> <sup id="anchor_01"><a href="#footnote_01">[1]</a></sup></td>
  <td>contains the <a href="https://pandoc.org/filters.html">Pandoc filters</a></td>
</tr>
<tr>
  <td style="min-width:170px;">&emsp;<a href="../data/filters/include-files.lua"><code>include-file.lua</code></a></td>
  <td><a href="https://pandoc.org/lua-filters.html">Lua filter</a> to include other Markdown files into the <i>master</i> document.</td>
</tr>
<tr style="background-color:#eeeeee;">
  <td><i><code>data/images/</code></i></td>
  <td>contains image files referenced in the Markdown files.</td>
</tr>
<tr>
  <td>&emsp;<a href="../data/images/external.png"><code>external.png</code></a></td>
  <td>PNG image file used to refer to the sibling online HTML page (image source listed in <a href="../data/images/external.txt"><code>external.txt</code></a>).</td>
</tr>
<tr>
  <td>&emsp;<a href="../data/images/scala-spiral.png"><code>scala-spiral.png</code></a></td>
  <td>PNG image file to be added on the title page</td>
</tr>
<tr style="background-color:#eeeeee;">
  <td><i><code>data/templates/</code></i></td>
  <td>contains syntax and template files</td>
</tr>
<tr>
  <td>&emsp;<a href="../data/templates/scala.xml"><code>scala.xml</code></a> <sup id="anchor_02"><a href="#footnote_02">[2]</a></sup></td>
  <td><a href="https://kate-editor.org/syntax/">Kate</a> syntax file defining the highlighting rules for the <a href="http://dotty.epfl.ch/docs/reference/syntax.html"><i>Scala 3 language</i></a>.</td>
</tr>
<tr>
  <td>&emsp;<a href="../data/templates/template.tex"><code>template.tex</code></a> <sup id="anchor_03"><a href="#footnote_03">[3]</a></sup></td>
  <td><a href="https://pandoc.org/MANUAL.html#templates">Pandoc template</a> file defining the layout of the generated PDF file.</td>
</tr>
<tr>
</table>

## <span id="footnotes">Footnotes</span>

<span id="footnote_01">[1]</span> ***Pandoc filters*** [↩](#anchor_01)

<dl><dd>
<a href="https://pandoc.org/filters.html">Pandoc filters</a> can be written in any programming language (<a href="<a href="https://pandoc.org/lua-filters.html">Lua</a>, <a href="https://metacpan.org/pod/Pandoc::Filter">Perl</a>, <a href="https://github.com/vinai/pandocfilters-php">PHP</a>, <a href="https://pypi.org/project/pandocfilters/">Python</a>, <a href="https://cran.r-project.org/web/packages/pandocfilters/">R</a>, etc.). However using a <a href="https://pypi.org/project/pandocfilters/">Python-based Pandoc filter</a>, for instance, requires a Python installation to be present. Starting with version 2.0, <a href="https://pandoc.org/">Pandoc</a> makes it possible to write <a href="https://pandoc.org/lua-filters.html">filters in Lua</a> without any external dependencies.
</dd></dl>

<span id="footnote_02">[2]</span> ***Kate syntax files*** [↩](#anchor_02)

<dl><dd>
File <a href="../data/templates/scala.xml"><code>data/templates/scala.xml</code></a> is derived from the Scala 2 syntax file <a href="https://github.com/KDE/syntax-highlighting/tree/master/data/syntax"><code>scala.xml</code></a> available from the <a href="https://github.com/KDE">KDE project</a>. Note that file <a href="../data/templates/scala.xml"><code>data/templates/scala.xml</code></a> <i>must be adapted</i> to any syntax change of the Scala 3 language.
</dd></dl>

<span id="footnote_03">[3]</span> ***Pandoc templates*** [↩](#anchor_03)

<dl><dd>
File <a href="../data/templates/template.tex"><code>data/templates/template.tex</code></a> is derived from the default TeX template bundled with <a href="https://pandoc.org/">Pandoc</a>; for instance it contains additional LaTeX code to set up the title page.<br/>The default TeX template file bundled with <a href="https://pandoc.org/">Pandoc</a> can be saved with the following command : <code>$PANDOC_HOME/pandoc -D latex &gt; default-template.tex</code>.
</dd></dl>

***

*[mics](https://github.com/michelou/)/January 2022* [**&#9650;**](#top "Back to top")
<span id="bottom">&nbsp;</span>

[github_dotty]: https://github.com/lampepfl/dotty/#dotty
[github_scala3doc]: https://github.com/lampepfl/dotty/tree/master/scala3doc#scala3doc
