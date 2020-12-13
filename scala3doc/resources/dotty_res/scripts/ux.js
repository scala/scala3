window.addEventListener("DOMContentLoaded", () => {
  var toggler = document.getElementById("leftToggler");
  if (toggler) {
    toggler.onclick = function () {
      document.getElementById("leftColumn").classList.toggle("open");
    };
  }

  var elements = document.getElementsByClassName("documentableElement")
  if (elements) {
    for (i = 0; i < elements.length; i++) {
      elements[i].onclick = function(){
        this.classList.toggle("expand")
      }
    }
  }

  $("#sideMenu2 span").on('click', function(){
    $(this).parent().toggleClass("expanded")
  });


  if (location.hash) {
    var selected = document.getElementById(location.hash.substring(1));
    if (selected){
      selected.classList.toggle("expand");
    }
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
