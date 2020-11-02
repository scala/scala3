$("#inheritance-diagram").ready(function() {
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
})
