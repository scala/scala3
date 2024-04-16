function currentChapter() {
  return parseInt(document.location.pathname.split('/').pop().substr(0, 2), 10);
}

function heading(i, heading, $heading) {
  const currentLevel = parseInt(heading.tagName.substring(1));

  if (currentLevel === this.headerLevel) {
    this.headerCounts[this.headerLevel]++;
  } else if (currentLevel < this.headerLevel) {
    while (currentLevel < this.headerLevel) {
      this.headerCounts[this.headerLevel] = 1;
      this.headerLevel--;
    }
    this.headerCounts[this.headerLevel]++;
  } else {
    while (currentLevel > this.headerLevel) {
      this.headerLevel++;
      this.headerCounts[this.headerLevel] = 1;
    }
  }
  return `${this.headerCounts[this.headerLevel]} ${$heading.text()}`;
}

// ignore when using wkhtmltopdf, or it won't work...
if (window.jekyllEnv !== 'spec-pdf') {
  $('#toc').toc(
    {
      'selectors': 'h1,h2,h3',
      'smoothScrolling': false,
      'chapter': currentChapter(),
      'headerLevel': 1,
      'headerCounts': [-1, currentChapter() - 1, 1, 1],
      'headerText': heading
    }
  );
}

// no language auto-detect so that EBNF isn't detected as scala
hljs.configure({
  languages: []
});

// KaTeX configuration
document.addEventListener("DOMContentLoaded", function() {
  renderMathInElement(document.body, {
    delimiters: [
      {left: "´", right: "´", display: false}, // "display: false" -> inline
      {left: "$$", right: "$$", display: true}
    ],
    ignoredTags: ['script', 'noscript', 'style', 'textarea'],
  });
  // syntax highlighting after KaTeX is loaded,
  // so that math can be used in code blocks
  hljs.initHighlighting();
  $("pre nobr").addClass("fixws");
  // point when all necessary js is done, so PDF to be rendered
  window.status = "loaded";
});

$("#chapters a").each(function (index) {
  const href = $(this).attr("href");
  $(this).toggleClass("chapter-active", document.location.pathname.endsWith(href));
});
