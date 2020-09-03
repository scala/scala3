window.addEventListener("DOMContentLoaded", () => {
    document.getElementById("leftToggler").onclick = function() {
        document.getElementById("leftColumn").classList.toggle("open");
    }
    hljs.registerLanguage('scala', highlightDotty);
    hljs.registerAliases(['dotty', 'scala3'], 'scala');
    hljs.initHighlighting();
  });
