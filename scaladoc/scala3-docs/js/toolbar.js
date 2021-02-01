$(document).ready(function() {
  $("#menu-icon").click(() => {
    $(".sidebar").toggleClass("toggled");
  })
  $("#search-icon").click(() => {
    $("#searchbar").toggleClass("shown");
    $("#search-api-input").focus();
  })
  const searchInput = $("#search-api-input");
  searchInput.keydown(evt => {
    if (evt.which == 13) {
      const baseUrl = $("#baseurl-input").val();
      window.location = (
        baseUrl + "/api/search.html?" +
        "searchTerm=" + searchInput.val() +
        "&previousUrl=" + encodeURI(window.location)
      );
    }
  })
})
