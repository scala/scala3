// Toggles a sidebar section
function toggleSection(titleElement) {
  const title = $(titleElement);
  title.siblings("ul").toggleClass("toggled");
  title.children("i.fas").toggleClass("fa-angle-right").toggleClass("fa-angle-down");
}
