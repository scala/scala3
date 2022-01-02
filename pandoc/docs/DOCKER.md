# <span id="top">More about Docker</span> <span style="size:25%;"><a href="../README.md" title="Back to README">↩</a></span>
<!-- created by mics (https://github.com/michelou/) on December 2020 -->

Command [`docker image inspect`][docker_inspect] helps us to get information out of the [`pandoc/ubuntu-latex`][docker_pandoc] image, e.g. to know the values of the [`ENV`][docker_env] and [`ENTRYPOINT`][docker_entrypoint] instructions :

<pre style="font-size:80%; max-width:680px;">
<b>&gt; <a href="https://docs.docker.com/engine/reference/commandline/image_inspect/">docker image inspect</a> --format "{{.Config.<b style="color:darkred;">Env</b>}}" pandoc/ubuntu-latex:2.11.4</b>
[PATH=/opt/texlive/texdir/bin/x86_64-linux:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin]
&nbsp;
<b>&gt; <a href="https://docs.docker.com/engine/reference/commandline/image_inspect/">docker image inspect</a> --format "{{.Config.<b style="color:darkred;">Entrypoint</b>}}" pandoc/ubuntu-latex:2.11.4</b>
[/usr/local/bin/pandoc]
</pre>

Let us now check that both commands [`pandoc`][pandoc_command] and [`lualatex`][lualatex] are available in the running container :

<pre style="font-size:80%; max-width:680px;">
<b>&gt; <a href="https://docs.docker.com/engine/reference/commandline/run/">docker run</a> --rm --entrypoint lualatex pandoc/ubuntu-latex:2.11.4 --version | findstr Version</b>
This is LuaHBTeX, Version 1.12.0 (TeX Live 2020)
&nbsp;
<b>&gt; <a href="https://docs.docker.com/engine/reference/commandline/run/">docker run</a> --rm --entrypoint which pandoc/ubuntu-latex:2.11.4 lualatex</b>
/opt/texlive/texdir/bin/x86_64-linux/lualatex
&nbsp;
<b>&gt; <a href="https://docs.docker.com/engine/reference/commandline/run/">docker run</a> --rm pandoc/ubuntu-latex:2.11.4 --version | findstr /b pandoc</b>
pandoc 2.11.4
</pre>

> **:mag_right:** We note with pleasure that tool versions are identical for MS Windows 10, Ubuntu 18.04 and the Docker image [`pandoc/ubuntu-latex`][docker_pandoc].

<!-- 
https://www.howtoinstall.me/ubuntu/18-04/xz-utils/
-->

At this point we face the same situation as in the previous environments where some fonts were missing.

We create a [`Dockerfile`](../Dockerfile) document which adds a layer with our additions on top on the [`pandoc/ubuntu-latex`][docker_pandoc] image. We reproduce below a simplified version :

<pre style="font-size:80%; max-width:600px;">
<a href="https://docs.docker.com/engine/reference/builder/#from"><b>FROM</b></a> <a href="https://hub.docker.com/r/pandoc/ubuntu-latex">pandoc/ubuntu-latex:2.11.4</a>
<a href="https://docs.docker.com/engine/reference/builder/#workdir"><b>WORKDIR</b></a> /app
<a href="https://docs.docker.com/engine/reference/builder/#copy"><b>COPY</b></a> ./data ./data
<a href="https://docs.docker.com/engine/reference/builder/#copy"><b>COPY</b></a> ./src ./src
<a href="https://docs.docker.com/engine/reference/builder/#copy"><b>COPY</b></a> <a href="../md2pdf">./md2pdf</a> .
[...]
<a href="https://docs.docker.com/engine/reference/builder/#env"><b>ENV</b></a> <span style="color:darkred;">TEXLIVE_HOME</span>=/opt/texlive/texdir
<a href="https://docs.docker.com/engine/reference/builder/#run"><b>RUN</b></a> echo "#!/bin/bash \n <a href="../md2pdf">./md2pdf</a>" > ./entrypoint.sh
<a href="https://docs.docker.com/engine/reference/builder/#run"><b>RUN</b></a> chmod +x ./entrypoint.sh
<a href="https://docs.docker.com/engine/reference/builder/#entrypoint" title="ENTRYPOINT instruction"><b>ENTRYPOINT</b></a> ["./entrypoint.sh"]
</pre>

Command [`docker images`][docker_images] displays the repository name, the tag name and the size of both images :

<pre style="font-size:80%; max-width:600px;">
<b>&gt; <a href="https://docs.docker.com/engine/reference/commandline/images/">docker images</a></b>
REPOSITORY               TAG         IMAGE ID       CREATED          SIZE
scala3/md2pdf            1.0         ec4bc8c273cd   19 minutes ago   861MB
pandoc/ubuntu-latex      2.11.4      fa20b6e6dfe6   5 days ago       799MB
</pre>

Finally, we can save both images as archive files with command [`docker save`][docker_save] (and [`gzip`][linux_gzip] for compression) :

<pre style="font-size:80%; max-width:600px;">
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/dir">dir</a> target\*tar* | <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/findstr">findstr</a> /b [0-9]</b>
29.01.2021  23:34       813 513 216 pandoc_ubuntu-latex-2.11.4.tar
29.01.2021  23:34       254 725 746 pandoc_ubuntu-latex-2.11.4.tar.gz
29.01.2021  23:31       876 669 952 scala3_md2pdf-1.0.tar
29.01.2021  23:31       283 584 449 scala3_md2pdf-1.0.tar.gz
</pre>

<!--
## <span id="footnotes">Footnotes</span>

<b name="footnote_01">[1]</b> ***Pandoc software*** [↩](#anchor_01)

<p style="margin:0 0 1em 20px;">
 (version: 2.11.3.2, compressed size: 240.96 MB)
</p>
-->
***

*[mics](https://github.com/michelou/)/January 2022* [**&#9650;**](#top "Back to top")
<span id="bottom">&nbsp;</span>

[docker_entrypoint]: https://docs.docker.com/engine/reference/builder/#entrypoint "ENTRYPOINT instruction"
[docker_env]: https://docs.docker.com/engine/reference/builder/#env
[docker_hub]: https://hub.docker.com/
[docker_images]: https://docs.docker.com/engine/reference/commandline/images/
[docker_inspect]: https://docs.docker.com/engine/reference/commandline/image_inspect/ "Display detailed information on one or more images"
[docker_pandoc]: https://hub.docker.com/r/pandoc/ubuntu-latex "Ubuntu based image"
[docker_save]: https://docs.docker.com/engine/reference/commandline/save/
[linux_gzip]: https://linux.die.net/man/1/gzip
[lualatex]: http://www.luatex.org/
[pandoc_command]: https://pandoc.org/MANUAL.html/ "Pandoc command line tool"
[tex_live]: https://tug.org/texlive/
[tex_live_repository]: https://github.com/TeX-Live/installer "TeX Live installer repository"
[tex_live_install]: https://tug.org/texlive/doc/install-tl.html "TeX Live cross-platform installer"
[tex_live_setup]: https://tug.org/texlive/acquire-netinstall.html
[tex_live_tug]: https://tug.org/ "TeX Users Group website"
[xelatex]: https://sourceforge.net/projects/xetex/
