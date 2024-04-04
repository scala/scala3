$(function() {
  const menuIcon = $("#menu-icon");
  const sidebar = $(".sidebar");
  menuIcon.on("click", () => {
    sidebar.toggleClass("toggled");
  });

  const searchIcon = $("#search-icon");
  const searchbar = $("#searchbar");
  const searchApiInput = $("#search-api-input");
  searchIcon.on("click", () => {
    searchbar.toggleClass("shown");
    searchApiInput.focus();
  });

  const baseurlInput = $("#baseurl-input");
  searchApiInput.keydown(evt => {
    if (evt.which === 13) { // Enter
      const baseUrl = baseurlInput.val();
      const searchTerm = searchApiInput.val();
      const previousUrl = encodeURI(window.location);
      const searchUrl = `${baseUrl}/api/search.html?searchTerm=${searchTerm}&previousUrl=${previousUrl}`;
      window.location = searchUrl;
    }
  });
});
