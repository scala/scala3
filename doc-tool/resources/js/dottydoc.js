$(document).ready(function() {
  // Code hilighting (requires hilight.js to be loaded)
  $('pre code').each(function(i, block) {
    hljs.highlightBlock(block);
  });
  // Autoscroll to anchor
  if (window.location.hash.length > 0) {
    window.scrollTo(0, $(window.location.hash).offset().top - 90);
  }
})
