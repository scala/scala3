// Toggles a sidebar section
function toggleSection(titleElement) {
  const title = $(titleElement);
  title.siblings("ul").toggle();
  title.children("i.fas").toggleClass("fa-angle-right").toggleClass("fa-angle-down");
}

// Unfolds the sidebar section corresponding to the current page
(function () {
  let activeEntry = document.querySelector("#active-toc-entry");
  while (activeEntry != null && activeEntry.tagName.toLowerCase() === "ul") {
    activeEntry.style.display = "block";
    activeEntry = activeEntry.parentElement.parentElement;
  }
})();
