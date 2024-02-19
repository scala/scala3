function currentChapter() {
  var path = document.location.pathname;
  var idx  = path.lastIndexOf("/") + 1;
  var chap = path.substring(idx, idx + 2);
  return parseInt(chap, 10);
}

function heading(i, heading, $heading) {
  var currentLevel = parseInt(heading.tagName.substring(1));
  var result = "";
  if (currentLevel === this.headerLevel) {
    this.headerCounts[this.headerLevel] += 1;
    return "" + this.headerCounts[this.headerLevel] + " " + $heading.text();
  } else if (currentLevel < this.headerLevel) {
    while(currentLevel < this.headerLevel) {
      this.headerCounts[this.headerLevel] = 1;
      this.headerLevel -= 1;
    }
    this.headerCounts[this.headerLevel] += 1;
    return "" + this.headerCounts[this.headerLevel]+ " " + $heading.text();
  } else {
    while(currentLevel > this.headerLevel) {
      this.headerLevel += 1;
      this.headerCounts[this.headerLevel] = 1;
    }
    return "" + this.headerCounts[this.headerLevel]+ " " + $heading.text();
  }
}

// ignore when using wkhtmltopdf, or it won't work...
if(window.jekyllEnv !== 'spec-pdf') {
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

// See https://github.com/highlightjs/highlight.js/issues/2889 for additional context.
function renderMath({el, result, text}) {

    const re = RegExp('´', 'g');

    const spans = [];

    // get the spans of all math elements
    while(match = re.exec(text)) {
	const start = match.index;
	match = re.exec(text);
	if(match == null) {
	    break;
	} else {
	    const end = match.index + 1;
	    spans.push({start, end});
	}
    }

    // render spans using katex
    for(const span of spans) {
	const {start, end} = span;
	const str = text.substring(start + 1, end - 1);
	const parent = new DocumentFragment();
	katex.render(str, parent, { throwOnError: false });
	const children = parent.children;
	// TODO: Double check mutiple elements aren't possible 
	if (children.length == 1) {
	    span.span = children[0];
	}
    }

    // Here we start the merging between the katex output and highlight output.
    if (spans.length != 0) {

	// This is essentially our iterator
	var offset = 0;
	var span = spans.shift();
	var child = el.firstChild;

	// highlight only supports one level of nesting.
	while (child) {
	    if (child instanceof Text) {
		const str = child.wholeText;
		const start = offset;
		const end = start + str.length;

		if (span.start >= start && span.end <= end) {
		    const beforeText = str.substring(0, span.start - start);
		    if (beforeText) {
			el.insertBefore(new Text(beforeText), child);
		    }
		    
		    const afterText = str.substring(span.end - start);
		    child = el.replaceChild(span.span, child);
		    
		    if (afterText) {
			const afterChild = new Text(afterText);
			if (child.nextSibling) {
			    el.insertBefore(afterChild, child.nextSibling);
			} else {
			    el.appendChild(afterChild);
			}
		    }
		    
		    offset = span.end;
		    
		} else {
		    offset = end;
		}
	    } else if (child.tagName) {
		const str = child.innerHTML;
		offset += str.length;
	    }

	    child = child.nextSibling;
	}
    }
}

hljs.addPlugin({
    'after:highlightElement': renderMath
});

// KaTeX configuration
document.addEventListener("DOMContentLoaded", function() {
  renderMathInElement(document.body, {
      delimiters: [
	  {left: "´", right: "´", display: false}, // "display: false" -> inline
	  {left: "$$", right: "$$", display: true}
      ],
      // We ignore 'code' here, because highlight will deal with it.
      ignoredTags: ['script', 'noscript', 'style', 'code'],
  });
  hljs.highlightAll();
  $("pre nobr").addClass("fixws");
  // point when all necessary js is done, so PDF to be rendered
  window.status = "loaded";
});

$("#chapters a").each(function (index) {
 if (document.location.pathname.endsWith($(this).attr("href")))
   $(this).addClass("chapter-active");
 else
   $(this).removeClass("chapter-active");
});
