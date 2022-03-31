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
      elements[i].onclick = function (e) {
        if (!$(e.target).is("a") && e.fromSnippet !== true)
          this.classList.toggle("expand")
      }
    }
  }

  $(".side-menu span").on('click', function () {
    $(this).parent().toggleClass("expanded")
  });

  const observer = new IntersectionObserver(entries => {
    entries.forEach(entry => {
      const id = entry.target.getAttribute('id');
      if (entry.intersectionRatio > 0) {
        document.querySelector(`#toc li a[href="#${id}"]`).parentElement.classList.add('active');
      } else {
        document.querySelector(`#toc li a[href="#${id}"]`).parentElement.classList.remove('active');
      }
    });
  });


  document.querySelectorAll('#content section[id]').forEach((section) => {
    observer.observe(section);
  });

  document.querySelectorAll(".side-menu a").forEach(elem => elem.addEventListener('click', e => e.stopPropagation()))

  $('.names .tab').on('click', function () {
    parent = $(this).parents(".tabs").first()
    shown = $(this).hasClass('selected')
    single = parent.hasClass("single")

    if (single) parent.find(".tab.selected").removeClass('selected')

    id = $(this).attr('data-togglable')
    myTab = parent.find("[data-togglable='" + id + "'].tab")
    if (!shown) { myTab.addClass('selected') }
    if (shown && !single) myTab.removeClass('selected')

    if (!shown && $(this).filter(".showGraph").length > 0) {
      showGraph()
      $(this).find(".showGraph").removeClass("showGraph")
    }
  })

  if (location.hash) {
    var target = location.hash.substring(1);
    // setting the 'expand' class on the top-level container causes undesireable styles
    // to apply to the top-level docs, so we avoid this logic for that element.
    if (target != 'container') {
      var selected = document.getElementById(location.hash.substring(1));
      if (selected) {
        selected.classList.toggle("expand");
      }
    }
  }

  var logo = document.getElementById("logo");
  if (logo) {
    logo.onclick = function () {
      window.location = pathToRoot; // global variable pathToRoot is created by the html renderer
    };
  }

  document.querySelectorAll('.documentableAnchor').forEach(elem => {
    elem.addEventListener('click', event => {
      var $temp = $("<input>")
      $("body").append($temp)
      var a = document.createElement('a')
      a.href = $(elem).attr("link")
      $temp.val(a.href).select();
      document.execCommand("copy")
      $temp.remove();
    })
  })

  hljs.registerLanguage("scala", highlightDotty);
  hljs.registerAliases(["dotty", "scala3"], "scala");

  var aliases = ['language-scala', 'language-dotty', 'language-scala3']

  var highlightDeep = function(el) {
    el.childNodes.forEach(node => {
      if(node.nodeType == Node.TEXT_NODE) {
        let newNode = document.createElement('span');
        newNode.innerHTML = hljs.highlight(node.textContent, {language: 'scala'}).value;
        el.insertBefore(newNode, node);
        el.removeChild(node);
      } else if(node.nodeType == Node.ELEMENT_NODE) {
        highlightDeep(node);
      }
    })
  }

  document.querySelectorAll('pre code').forEach( el => {
    if (aliases.some(alias => el.classList.contains(alias))) {
      highlightDeep(el);
    } else {
      hljs.highlightElement(el);
    }
  });


  /* listen for the `F` key to be pressed, to focus on the member filter input (if it's present) */
  document.body.addEventListener('keydown', e => {
    if (e.key == "f") {
      const tag = e.target.tagName;
      if (tag != "INPUT" && tag != "TEXTAREA") {
        const filterInput = findRef('.documentableFilter input.filterableInput');
        if (filterInput != null) {
          // if we focus during this event handler, the `f` key gets typed into the input
          setTimeout(() => filterInput.focus(), 1);
        }
      }
    }
  })
});

var zoom;
var transform;

function showGraph() {
  if ($("svg#graph").children().length == 0) {
    var dotNode = document.querySelector("#dot")
    if (dotNode) {
      var svg = d3.select("#graph");
      var radialGradient = svg.append("defs").append("radialGradient").attr("id", "Gradient");
      radialGradient.append("stop").attr("stop-color", "var(--aureole)").attr("offset", "20%");
      radialGradient.append("stop").attr("stop-color", "var(--code-bg)").attr("offset", "100%");

      var inner = svg.append("g");

      // Set up zoom support
      zoom = d3.zoom()
        .on("zoom", function ({ transform }) {
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
          style: g.node(v).style,
          id: g.node(v).id
        });
      });
      g.setNode("node0Cluster", {
        style: "fill: url(#Gradient);",
        id: "node0Cluster"
      });
      g.setParent("node0", "node0Cluster");

      g.edges().forEach(function (v) {
        g.setEdge(v, {
          arrowhead: "vee"
        });
      });
      render(inner, g);

      // Set the 'fit to content graph' upon landing on the page
      var bounds = svg.node().getBBox();
      var parent = svg.node().parentElement;
      var fullWidth = parent.clientWidth || parent.parentNode.clientWidth,
        fullHeight = parent.clientHeight || parent.parentNode.clientHeight;
      var width = bounds.width,
        height = bounds.height;
      var midX = bounds.x + width / 2,
        midY = bounds.y + height / 2;
      if (width == 0 || height == 0) return; // nothing to fit
      var scale = Math.min(fullWidth / width, fullHeight / height) * 0.99; // 0.99 to make a little padding
      var translate = [fullWidth / 2 - scale * midX, fullHeight / 2 - scale * midY];

      transform = d3.zoomIdentity
        .translate(translate[0], translate[1])
        .scale(scale);

      svg.call(zoom.transform, transform);

      // This is nasty hack to prevent DagreD3 from stretching cluster. There is similar issue on github since October 2019, but haven't been answered yet. https://github.com/dagrejs/dagre-d3/issues/377
      var node0 = d3.select("g#node0")._groups[0][0];
      var node0Rect = node0.children[0];
      var node0Cluster = d3.select("g#node0Cluster")._groups[0][0];
      var node0ClusterRect = node0Cluster.children[0];
      node0Cluster.setAttribute("transform", node0.getAttribute("transform"));
      node0ClusterRect.setAttribute("width", +node0Rect.getAttribute("width") + 80);
      node0ClusterRect.setAttribute("height", +node0Rect.getAttribute("height") + 80);
      node0ClusterRect.setAttribute("x", node0Rect.getAttribute("x") - 40);
      node0ClusterRect.setAttribute("y", node0Rect.getAttribute("y") - 40);
    }
  }
}

function zoomOut() {
  var svg = d3.select("#graph");
  svg
    .transition()
    .duration(2000)
    .call(zoom.transform, transform);
}
