window.addEventListener("DOMContentLoaded", () => {
  var toggler = document.getElementById("leftToggler");
  if (toggler) {
    toggler.onclick = function () {
      document.getElementById("leftColumn").classList.toggle("open");
    };
  }
  var logo = document.getElementById("logo");
  if (logo) {
    logo.onclick = function() {
      window.location = pathToRoot; // global variable pathToRoot is created by the html renderer
    };
  }
  hljs.registerLanguage("scala", highlightDotty);
  hljs.registerAliases(["dotty", "scala3"], "scala");
  hljs.initHighlighting();
});
