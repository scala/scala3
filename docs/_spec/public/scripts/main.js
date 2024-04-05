function currentChapter() {
  return parseInt(document.location.pathname.split('/').pop().substr(0, 2), 10);
}

function heading(i, heading, $heading) {
  const currentLevel = parseInt(heading.tagName.substring(1));
  const headerCounts = this.headerCounts;
  let result = "";
  if (currentLevel === this.headerLevel) {
    headerCounts[this.headerLevel]++;
    result = `${headerCounts[this.headerLevel]} ${$heading.text()}`;
  } else if (currentLevel < this.headerLevel) {
    while (currentLevel < this.headerLevel) {
      headerCounts[this.headerLevel] = 1;
      this.headerLevel--;
    }
    headerCounts[this.headerLevel]++;
    result = `${headerCounts[this.headerLevel]} ${$heading.text()}`;
  } else {
    while (currentLevel > this.headerLevel) {
      this.headerLevel++;
      headerCounts[this.headerLevel] = 1;
    }
    result = `${headerCounts[this.headerLevel]} ${$heading.text()}`;
  }
  return result;
}

if (window.jekyllEnv !== 'spec-pdf') {
  $('#toc').toc({
    selectors: 'h1,h2,h3',
    smoothScrolling: false,
    chapter: currentChapter(),
    headerLevel: 1,
    headerCounts: [-1, currentChapter() - 1, 1, 1],
    headerText: heading
  });
}

hljs.configure({
  languages: []
});

document.addEventListener("DOMContentLoaded", function() {
  renderMathInElement(document.body, {
    delimiters: [
      {left: "´", right: "´", display: false}, // "display: false" -> inline
      {left: "$$", right: "$$", display: true}
    ],
    ignoredTags: ['script', 'noscript', 'style', 'textarea'],
  });
  hljs.initHighlighting();
  $("pre nobr").addClass("fixws");
  window.status = "loaded";
});

$("#chapters a").each(function (index) {
  const href = $(this).attr("href");
  $(this).toggleClass("chapter-active", document.location.pathname.endsWith(href));
});
