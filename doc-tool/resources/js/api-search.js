onmessage = function(e) {
  var docs = e.data.docs;
  var searchTerm = e.data.search;
  postMessage(
    "Search not implemented, couldn't find: " + searchTerm
  );
}
