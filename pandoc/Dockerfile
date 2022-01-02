## Created by mics (https://github.com/michelou/) on December 2020.

FROM pandoc/ubuntu-latex:2.11.4

LABEL md2pdf.author="Stéphane Micheloud <stephane.micheloud@gmail.com>"
LABEL md2pdf.description="Create PDF file for Scala 3 documentation"
LABEL md2pdf.version=1.0

WORKDIR /dotty
COPY ./tmp/docs ./docs/docs

WORKDIR pandoc
COPY ./data ./data
COPY ./md2pdf .

ENV TEXLIVE_HOME=/opt/texlive/texdir

## See https://github.com/openjournals/whedon/blob/master/paperdraft.Dockerfile
RUN tlmgr update --self

## Install additional LaTeX packages
## Already present: fancyvrb, fontspec, l3backend, l3kernel, l3packages, pgf, unicode-math, xcolor.
RUN tlmgr install \
 dejavu \
 environ \
 gnu-freefont \
 selnolig \
 tcolorbox \
 trimspaces

RUN echo "#!/bin/bash \n ./md2pdf \n ls /dotty/out/pandoc/" > ./entrypoint.sh
RUN chmod +x ./entrypoint.sh

ENTRYPOINT [ "./entrypoint.sh" ]
CMD [ "reference" ]
