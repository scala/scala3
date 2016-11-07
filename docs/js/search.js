(function() {
  var searchTerm = getQueryVariable('query');
  function showMark(first, last) {
      var res = "";
      if ((first.length + last.length) <= 150) {
        res = '<p class="result-text">' + first + "<mark>" + searchTerm + "</mark>" + last + '...</p></li>';
      } else if (first.length > 150 && last.length > 150) {
        res = '<p class="result-text">' + first.substring(first.length - 76, first.length - 1) + "<mark>" + searchTerm + "</mark>" + last.substring(0, 75) + '...</p></li>';
      } else if (first.length > last.length) {
        var lastLen = Math.min(last.length, 75);
        var frstLen = Math.min(first.length, 150 - lastLen);
        res = '<p class="result-text">' + first.substring(first.length - frstLen - 1, first.length - 1) + "<mark>" + searchTerm + "</mark>" + last.substring(0, 75) + '...</p></li>';
      }
      return res;
  }

  function displaySearchResults(results, store) {
    var searchResults = document.getElementById('search-results');

    if (results.length) { // Are there any results?
      var appendString = '';

      for (var i = 0; i < results.length; i++) {  // Iterate over the results
        var item = store[results[i].ref];
        appendString += '<li><a href="' + item.url + "?highlight=" + searchTerm + '"><h3>' + item.title + '</h3></a>';

        var text = item.content.split(searchTerm)

        if (text.length == 1) {
          appendString += '<p class="result-text">' + item.content.substring(0, 150) + '...</p></li>';
        }
        else if (text.length == 2) {
          appendString += showMark(text[0], text[1]);
        }
        else {
          for (var j = 0; j < text.length - 1; j++) {
            appendString += showMark(text[j], text[j+1]);
          }
        }
      }

      searchResults.innerHTML = appendString;
    } else {
      searchResults.innerHTML = '<li>No results found</li>';
    }
  }

  function getQueryVariable(variable) {
    var query = window.location.search.substring(1);
    var vars = query.split('&');

    for (var i = 0; i < vars.length; i++) {
      var pair = vars[i].split('=');

      if (pair[0] === variable) {
        return decodeURIComponent(pair[1].replace(/\+/g, '%20'));
      }
    }
  }

  if (searchTerm) {
    document.getElementById('search-box').setAttribute("value", searchTerm);
    var idx = elasticlunr(function () {
      this.addField('title'); // hits in title get a boost
      this.addField('author');
      this.addField('content');
      this.setRef('id');
    });

    for (var key in window.store) { // Add the data to lunr
      var author = (typeof window.store[key].author === 'undefined') ? "" : window.store[key].author;
      idx.addDoc({
        'id': key,
        'title': window.store[key].title,
        'author': author,
        'content': window.store[key].content
      });

      var results = idx.search(searchTerm, {
          fields: {
              title:   { boost: 3 },
              author:  { boost: 2 },
              content: { boost: 1 }
          }
      }); // Get lunr to perform a search
      displaySearchResults(results, window.store); // We'll write this in the next section
    }
  }
})();
