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

  $('.names .tab').on('click', function(){
    parent = $(this).parents(".tabs").first()
    shown = $(this).hasClass('selected')
    single = parent.hasClass("single")

    if (single) parent.find(".tab.selected").removeClass('selected')

    id = $(this).attr('data-togglable')
    myTab = parent.find("[data-togglable='" + id + "'].tab")
    if (!shown) { myTab.addClass('selected') }
    if (shown && !single) myTab.removeClass('selected')

    if(!shown && $(this).find(".showGraph")){
      showGraph()
      $(this).find(".showGraph").removeClass("showGraph")
    }
  })

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

function showGraph() {
  if ($("svg#graph").children().length == 0) {
    var dotNode = document.querySelector("#dot")
    if (dotNode){
      var svg = d3.select("#graph");
      var inner = svg.append("g");

      // Set up zoom support
      var zoom = d3.zoom()
        .on("zoom", function({transform}) {
          inner.attr("transform", transform);
        });
      svg.call(zoom);

      var render = new dagreD3.render();
      var g = graphlibDot.read(dotNode.text);
      g.graph().rankDir = 'BT';
      g.nodes().forEach(function (v) {
        g.setNode(v, {
          labelType: "html",
          label: g.node(v).label,
          style: g.node(v).style
        });
      });
      g.edges().forEach(function(v) {
        g.setEdge(v, {
          arrowhead: "vee"
        });
      });
      render(inner, g);
    }
  }
}
