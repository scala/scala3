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

    console.log(text);

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
	if (children.length == 1) {
	    span.span = children[0];
	}
    }

    function go(parent, offset, spans) {
	
	var span = spans.shift();
	var child = parent.firstChild;
	
	while (child && span) {

	    if (child instanceof Text) {
		const str = child.wholeText;
		const start = offset;
		const end = start + str.length;
		
		// TODO: Deal with the other situation, where our math span encompasses multiple highlight spans...
		if (span.start >= start && span.end <= end) {
		    const beforeText = str.substring(0, span.start - start);
		    if (beforeText) {
			parent.insertBefore(new Text(beforeText), child);
		    }
		    const afterText = str.substring(span.end - start);
		    const newChild = span.span;
		    parent.replaceChild(newChild, child);
		    child = newChild;
		    const afterChild = new Text(afterText);
		    const next = child.nextSibling;
		    if (next) {
			parent.insertBefore(afterChild, next);
		    } else {
			parent.appendChild(afterChild);
		    }
		    offset = span.end;
		    span = spans.shift();
		} else {
		    offset = end;
		}
	    } else if (child.tagName == "SPAN") {
		spans.unshift(span);
		offset = go(child, offset, spans);
	    } else {
		console.log("skipping element");
	    }

	    while (span && offset > span.end) {
		console.log("dropping span...");
		span = spans.shift();
	    }

	    child = child.nextSibling;
	}

	return offset;
    }

    // Here we start the merging between the katex output and highlight output.
    if (spans.length != 0) {
	go(el, 0, spans);
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
  hljs.configure({ cssSelector: 'code' });
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
