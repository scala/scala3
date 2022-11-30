let observer = null;

const attrsToCopy = [
  "data-githubContributorsUrl",
  "data-githubContributorsFilename",
  "data-pathToRoot",
]

/**
 * @typedef {Object} SavedPageState
 * @property {Strign} mainDiv
 * @property {String} leftColumn
 * @property {String} title
 * @property {Record<string, string>} attrs
 */

/**
 * @param {Document} doc
 * @returns {SavedPageState}
 */
function savePageState(doc) {
  const attrs = {};
  for (const attr of attrsToCopy) {
    attrs[attr] = doc.documentElement.getAttribute(attr);
  }
  return {
    mainDiv: doc.querySelector("#main")?.innerHTML,
    leftColumn: doc.querySelector("#leftColumn").innerHTML,
    title: doc.title,
    attrs,
  };
}

/**
 * @param {Document} doc
 * @param {SavedPageState} saved
 */
function loadPageState(doc, saved) {
  doc.title = saved.title;
  doc.querySelector("#main").innerHTML = saved.mainDiv;
  doc.querySelector("#leftColumn").innerHTML = saved.leftColumn;
  for (const attr of attrsToCopy) {
    doc.documentElement.setAttribute(attr, saved.attrs[attr]);
  }
}

function attachAllListeners() {
  if (observer) {
    observer.disconnect();
  }

  var anyNodeExpanded = document.querySelectorAll(".ni.n0.expanded").length > 0;
  var firstNavNode = document.querySelector(".ni.n0");
  if (!anyNodeExpanded && firstNavNode != null) {
    var firstNavNodeAddress = firstNavNode.querySelector("a");
    firstNavNode.classList.add("expanded");
    var button = firstNavNode.querySelector("button.ar");
    if (button != null) {
      button.classList.add("expanded");
    }
  }

  var scrollPosition = sessionStorage.getItem("scroll_value");
  if (scrollPosition) {
    var sideMenu = document.querySelector(".side-menu");
    sideMenu.scrollTo(0, scrollPosition);
  }

  const currentLocationHash = window.location.hash;

  const currentSection = [
    ...document.querySelectorAll("#content section[id]"),
  ].find((section) => currentLocationHash === `#${section.id}`);

  if (currentSection) {
    document.querySelector("#main").scrollTo(0, currentSection.offsetTop - 100);
  }

  var elements = document.getElementsByClassName("documentableElement");
  if (elements) {
    for (i = 0; i < elements.length; i++) {
      var expanderChild = elements[i].querySelector(
        ".documentableElement-expander",
      );
      if (
        elements[i].querySelector(".show-content") !== null &&
        expanderChild !== null
      ) {
        expanderChild.onclick = function (e) {
          if (!$(e.target).is("a") && e.fromSnippet !== true) {
            this.parentElement.classList.toggle("expand");
            this.children[0].classList.toggle("expanded");
            this.querySelector(".show-content").classList.toggle("expand");
          }
        };
      }
    }
  }

document
  .querySelectorAll(".documentableElement .signature")
  .forEach((signature) => {
    const short = signature.querySelector(".signature-short");
    const long = signature.querySelector(".signature-long");
    const extender = document.createElement("span");
    const extenderDots = document.createTextNode("...");
    extender.appendChild(extenderDots);
    extender.classList.add("extender");
    if (short && long && signature.children[1].hasChildNodes()) {
      signature.children[0].append(extender);
    }
  });

  const documentableLists = document.getElementsByClassName("documentableList");
  [...documentableLists].forEach((list) => {
    list.children[0].addEventListener("click", () => {
      list.classList.toggle("expand");
      list.children[0].children[0].classList.toggle("expand");
    });
  });

  var memberLists = document.getElementsByClassName("tab");
  if (memberLists) {
    for (i = 0; i < memberLists.length; i++) {
      if ($(memberLists[i].children[0].children[0]).is("button")) {
        memberLists[i].children[0].onclick = function (e) {
          this.classList.toggle("expand");
          this.children[0].classList.toggle("expand");
          this.parentElement.classList.toggle("expand");
          this.parentElement.parentElement.classList.toggle("expand");
        };
      }
    }
  }

  const documentableBriefs = document.querySelectorAll(".documentableBrief");
  [...documentableBriefs].forEach((brief) => {
    brief.addEventListener("click", () => {
      brief.parentElement.parentElement.parentElement.parentElement.classList.add(
        "expand",
      );
      brief.parentElement.parentElement.parentElement.previousElementSibling.children[0].classList.add(
        "expanded",
      );
    });
  });

  document.querySelectorAll("a").forEach((el) => {
    const href = el.href;
    if (href === "") {
      return;
    }
    const url = new URL(href);
    el.addEventListener("click", (e) => {
      if (
        url.href.replace(/#.*/, "") === window.location.href.replace(/#.*/, "")
      ) {
        return;
      }
      if (url.origin !== window.location.origin) {
        return;
      }
      if (e.metaKey || e.ctrlKey || e.shiftKey || e.altKey || e.button !== 0) {
        return;
      }
      e.preventDefault();
      e.stopPropagation();
      $.get(href, function (data) {
        if (window.history.state === null) {
          window.history.replaceState(savePageState(document), "");
        }
        const parser = new DOMParser();
        const parsedDocument = parser.parseFromString(data, "text/html");
        const state = savePageState(parsedDocument);
        window.history.pushState(state, "", href);
        loadPageState(document, state);
        window.dispatchEvent(new Event(DYNAMIC_PAGE_LOAD));
        document
          .querySelector("#main")
          .scrollTo({ top: 0, left: 0, behavior: "instant" });
      });
    });
  });

  $(".ar").on("click", function (e) {
    $(this).parent().parent().toggleClass("expanded");
    $(this).toggleClass("expanded");
    e.stopPropagation();
  });

  document.querySelectorAll(".documentableList .ar").forEach((arrow) => {
    arrow.addEventListener("click", () => {
      arrow.parentElement.parentElement.classList.toggle("expand");
      arrow.classList.toggle("expand");
    });
  });

  document.querySelectorAll(".nh").forEach((el) =>
    el.addEventListener("click", () => {
      if (
        el.lastChild.href.replace("#", "") ===
        window.location.href.replace("#", "")
      ) {
        el.parentElement.classList.toggle("expanded");
        el.firstChild.classList.toggle("expanded");
      } else {
        el.lastChild.click();
      }
    }),
  );

  const toggleShowAllElem = (element) => {
    if (element.textContent == "Show all") {
      element.textContent = "Collapse";
    } else {
      element.textContent = "Show all";
    }
  };

  document.querySelectorAll(".supertypes").forEach((el) =>
    el.lastElementChild.addEventListener("click", () => {
      el.classList.toggle("collapsed");
      toggleShowAllElem(el.lastElementChild);
    }),
  );

  document.querySelectorAll(".subtypes").forEach((el) =>
    el.lastElementChild.addEventListener("click", () => {
      el.classList.toggle("collapsed");
      toggleShowAllElem(el.lastElementChild);
    }),
  );

  document.querySelectorAll(".ni").forEach((link) =>
    link.addEventListener("mouseenter", (_e) => {
      sessionStorage.setItem(
        "scroll_value",
        link.offsetTop - window.innerHeight / 2,
      );
    }),
  );

  const getIdOfElement = (element) => element.target.getAttribute("id");
  const getTocListElement = (selector) =>
    document.querySelector(`#toc li a[href="#${selector}"]`);

  const tocHashes = [...document.querySelectorAll("#toc li a")].reduce(
    (acc, link) => {
      if (link.hash.length) {
        acc.push(link.hash);
      }
      return acc;
    },
    [],
  );

  const removeAllHighlights = () => {
    tocHashes.forEach((hash) => {
      const element = document.querySelector(`#toc li a[href="${hash}"]`);
      if (element.parentElement?.classList?.contains("active")) {
        element.parentElement.classList.remove("active");
      }
    });
  };

  observer = new IntersectionObserver(
    (entries) => {
      const firstEntry = entries[0];
      const lastEntry = entries[entries.length - 1];

      const currentHash = window.location.hash;

      const element = document.querySelector(
        `#toc li a[href="${currentHash}"]`,
      );
      if (element) {
        removeAllHighlights();
        element.parentElement?.classList.toggle("active");
      }

      if (entries.length > 3) {
        removeAllHighlights();
        const id = getIdOfElement(firstEntry);

        getTocListElement(id).parentElement.classList.toggle("active");
      }
      if (lastEntry.isIntersecting) {
        window.location.hash = "";
        removeAllHighlights();
        const id = getIdOfElement(lastEntry);

        getTocListElement(id).parentElement.classList.toggle("active");
      }
    },
    {
      rootMargin: "-10% 0px -50%",
    },
  );

  document.querySelectorAll("#content section[id]").forEach((section) => {
    observer.observe(section);
  });

  if (location.hash) {
    var target = location.hash.substring(1);
    // setting the 'expand' class on the top-level container causes undesireable styles
    // to apply to the top-level docs, so we avoid this logic for that element.
    if (target != "container") {
      var selected = document.getElementById(location.hash.substring(1));
      if (selected) {
        selected.classList.toggle("expand");
      }
    }
  }

  document.querySelectorAll("pre code").forEach((el) => {
    hljs.highlightBlock(el);
  });

  /* listen for the `F` key to be pressed, to focus on the member filter input (if it's present) */
  document.body.addEventListener("keydown", (e) => {
    if (e.key == "f") {
      const tag = e.target.tagName;
      if (tag != "INPUT" && tag != "TEXTAREA") {
        const filterInput = findRef(
          ".documentableFilter input.filterableInput",
        );
        if (filterInput != null) {
          // if we focus during this event handler, the `f` key gets typed into the input
          setTimeout(() => filterInput.focus(), 1);
        }
      }
    }
  });

  // when document is loaded graph needs to be shown
}

const DYNAMIC_PAGE_LOAD = "dynamicPageLoad";
window.addEventListener(DYNAMIC_PAGE_LOAD, () => {
  attachAllListeners();
});

window.addEventListener("dynamicPageLoad", () => {
  const sideMenuOpen = sessionStorage.getItem("sideMenuOpen");
  if (sideMenuOpen) {
    if (document.querySelector("#leftColumn").classList.contains("show")) {
      document.querySelector("#content").classList.add("sidebar-shown");
    }
    sessionStorage.removeItem("sideMenuOpen");
  } else {
    const leftColumn = document.querySelector(".show");
    if (leftColumn) leftColumn.classList.remove("show");

    const mobileSidebarToggleButton = document.querySelector(".menu-shown");
    if (mobileSidebarToggleButton)
      mobileSidebarToggleButton.classList.remove("menu-shown");

    const content = document.querySelector(".sidebar-shown");
    if (content) content.classList.remove("sidebar-shown");
  }
});

window.addEventListener("DOMContentLoaded", () => {
  hljs.registerLanguage("scala", highlightDotty);
  hljs.registerAliases(["dotty", "scala3"], "scala");
  window.dispatchEvent(new Event(DYNAMIC_PAGE_LOAD));
});

const elements = document.querySelectorAll(".documentableElement");

// show/hide side menu on mobile view
const sideMenuToggler = document.getElementById("mobile-sidebar-toggle");
sideMenuToggler.addEventListener("click", (_e) => {
  document.getElementById("leftColumn").classList.toggle("show");
  document.getElementById("content").classList.toggle("sidebar-shown");
  const toc = document.getElementById("toc");
  if (toc && toc.childElementCount > 0) {
    toc.classList.toggle("sidebar-shown");
  }
  sideMenuToggler.classList.toggle("menu-shown");
});

// show/hide mobile menu on mobile view
document
  .getElementById("mobile-menu-toggle")
  .addEventListener("click", (_e) => {
    document.getElementById("mobile-menu").classList.add("show");
  });
document.getElementById("mobile-menu-close").addEventListener("click", (_e) => {
  document.getElementById("mobile-menu").classList.remove("show");
});

window.addEventListener("popstate", (e) => {
  if (e.state === null) {
    return;
  }
  loadPageState(document, e.state);
  window.dispatchEvent(new Event(DYNAMIC_PAGE_LOAD));
});

var zoom;
var transform;

function showGraph() {
  document.getElementById("inheritance-diagram").classList.add("shown");
  if ($("svg#graph").children().length == 0) {
    var dotNode = document.querySelector("#dot");

    if (dotNode) {
      var svg = d3.select("#graph");
      var radialGradient = svg
        .append("defs")
        .append("radialGradient")
        .attr("id", "Gradient");
      radialGradient
        .append("stop")
        .attr("stop-color", "var(--yellow9)")
        .attr("offset", "30%");
      radialGradient
        .append("stop")
        .attr("stop-color", "var(--background-default)")
        .attr("offset", "100%");

      var inner = svg.append("g");

      // Set up zoom support
      zoom = d3.zoom().on("zoom", function ({ transform }) {
        inner.attr("transform", transform);
      });
      svg.call(zoom);

      var render = new dagreD3.render();
      var g = graphlibDot.read(dotNode.text);
      g.graph().rankDir = "BT";
      g.nodes().forEach(function (v) {
        g.setNode(v, {
          labelType: "html",
          label: g.node(v).label,
          class: g.node(v).class,
          id: g.node(v).id,
          rx: "4px",
          ry: "4px",
        });
      });
      g.setNode("node0Cluster", {
        style: "fill: url(#Gradient);",
        id: "node0Cluster",
      });
      g.setParent("node0", "node0Cluster");

      g.edges().forEach(function (v) {
        g.setEdge(v, {
          arrowhead: "hollowPoint",
        });
      });

      render.arrows().hollowPoint = function normal(parent, id, edge, type) {
        var marker = parent
          .append("marker")
          .attr("id", id)
          .attr("viewBox", "0 0 10 10")
          .attr("refX", 9)
          .attr("refY", 5)
          .attr("markerUnits", "strokeWidth")
          .attr("markerWidth", 12)
          .attr("markerHeight", 12)
          .attr("orient", "auto");

        var path = marker
          .append("path")
          .attr("d", "M 0 0 L 10 5 L 0 10 z")
          .style("stroke-width", 1)
          .style("stroke-dasharray", "1,0")
          .style("fill", "var(--grey12)")
          .style("stroke", "var(--grey12)");
        dagreD3.util.applyStyle(path, edge[type + "Style"]);
      };

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
      var translate = [
        fullWidth / 2 - scale * midX,
        fullHeight / 2 - scale * midY,
      ];

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
      node0ClusterRect.setAttribute(
        "width",
        +node0Rect.getAttribute("width") + 80,
      );
      node0ClusterRect.setAttribute(
        "height",
        +node0Rect.getAttribute("height") + 80,
      );
      node0ClusterRect.setAttribute("x", node0Rect.getAttribute("x") - 40);
      node0ClusterRect.setAttribute("y", node0Rect.getAttribute("y") - 40);
    }
  }
}

function hideGraph() {
  document.getElementById("inheritance-diagram").classList.remove("shown");
}

function zoomOut() {
  var svg = d3.select("#graph");
  svg.transition().duration(2000).call(zoom.transform, transform);
}

const members = [...document.querySelectorAll("[id]")];
members.forEach((member) => {
  window.addEventListener("resize", () => {
    const navbarHeight = document.querySelector("#header").clientHeight;
    const filtersHeight = document.querySelector(
      ".documentableFilter",
    )?.clientHeight;
    if (navbarHeight && filtersHeight) {
      member.style.scrollMarginTop = `${navbarHeight + filtersHeight}px`;
    }
  });
});

members.forEach((member) => {
  window.addEventListener("DOMContentLoaded", () => {
    const navbarHeight = document.querySelector("#header").clientHeight;
    const filtersHeight = document.querySelector(
      ".documentableFilter",
    )?.clientHeight;
    if (navbarHeight && filtersHeight) {
      member.style.scrollMarginTop = `${navbarHeight + filtersHeight}px`;
    }
  });
});

window.addEventListener(DYNAMIC_PAGE_LOAD, () => {
  const docsLink = document.querySelector("#docs-nav-button");
  const apiLink = document.querySelector("#api-nav-button");

  docsLink &&
    apiLink &&
    [docsLink, apiLink].forEach((button) => {
      button.addEventListener("click", () => {
        sessionStorage.setItem("sideMenuOpen", true);
      });
    });
});