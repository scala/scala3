window.addEventListener("DOMContentLoaded", () => {
    var e = document.getElementById("leftToggler");
    if (e) {
        e.onclick = function() {
            document.getElementById("leftColumn").classList.toggle("open");
        };
    }
    hljs.registerLanguage('scala', highlightDotty);
    hljs.registerAliases(['dotty', 'scala3'], 'scala');
    hljs.initHighlighting();
  });
