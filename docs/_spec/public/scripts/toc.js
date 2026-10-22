/*!
 * toc - jQuery Table of Contents Plugin
 * v0.3.2
 * http://projects.jga.me/toc/
 * copyright Greg Allen 2014
 * MIT License
*/

/* The following ports the jquery plugin to modern javascript. */

/***
 * Add table of contents links to the given TOC container.
 * @param {HTMLElement} domElement - The DOM element to manipulate
 * @param {Object} options - A list of options to change TOC_DEFAULTS.
 * @return {HTMLElement} - The same tocContainer for chaining.
 */
function addTOC(tocContainer, options) {
  var verboseIdCache = {};
  // Since options are only one level of nesting deep,
  // we use the spread syntax to merge.
  var opts = {...TOC_DEFAULTS, ...options};
  var container = document.querySelector(opts.container);
  var headings = container.querySelectorAll(opts.selectors);
  var headingOffsets = [];
  var activeClassName = opts.activeClass;
  var scrollTo = function(e, callback) {
    tocContainer.querySelectorAll('li').forEach(function (element) {
      element.classList.remove(activeClassName);
    });
    e.parentNode.classList.add(activeClassName);
  };

  var timeout;
  var highlightOnScroll = function(e) {
    if (timeout) {
      clearTimeout(timeout);
    }
    timeout = setTimeout(function() {
      var top = window.pageYOffset,
        highlighted, closest = Number.MAX_VALUE, index = 0;
      for (var i = 0, c = headingOffsets.length; i < c; i++) {
        var currentClosest = Math.abs(headingOffsets[i] - top);
        if (currentClosest < closest) {
          index = i;
          closest = currentClosest;
        }
      }
      tocContainer.querySelectorAll('li').forEach(function (element) {
        element.classList.remove(activeClassName);
      });
      highlighted = tocContainer.querySelector('li:nth-child(' + index + ')')
      if (highlighted != null) { highlighted.classList.add(activeClassName); }
      opts.onHighlight(highlighted);
    }, 50);
  };

  if (opts.highlightOnScroll) {
    window.addEventListener('scroll', highlightOnScroll);
    highlightOnScroll();
  }

  const listTypeRegex = new RegExp("<(.*)/>");
  const listTypeToTagName = listTypeRegex.exec(opts.listType);
  if (listTypeToTagName == null) { return tocContainer; }
  var ul = document.createElement(listTypeToTagName[1]);
  headings.forEach(function(heading, i) {
    headingOffsets.push(heading.getBoundingClientRect().top - opts.highlightOffset);
    var anchorName = opts.anchorName(i, heading, opts.prefix);
    if(heading.id !== anchorName) {
      var anchor = document.createElement("span");
      anchor.setAttribute('id', anchorName);
      heading.parentNode.insertBefore(anchor, heading);
    }

    var a = document.createElement("a");
    a.textContent = opts.headerText(i, heading);
    a.setAttribute('href', '#' + anchorName);
    a.addEventListener('click', () => {
      window.removeEventListener('scroll', highlightOnScroll);
      scrollTo(a, function() {
        // Kept bug from jquery version: callback isn't called, so scroll event listener not re-attached.
        window.addEventListener('scroll', highlightOnScroll);
      });
      // Research suggests this is a now unused custom event. Won't translate.
      // el.trigger('selected', $(this).attr('href'))
    });
    var li = document.createElement('li');
    li.classList.add(opts.itemClass(i, heading, opts.prefix));
    li.append(a);
    ul.append(li);
  });
  tocContainer.replaceChildren(ul);
}

const TOC_DEFAULTS = {
  container: 'body',
  listType: '<ul/>',
  selectors: 'h1,h2,h3',
  prefix: 'toc',
  activeClass: 'toc-active',
  onHighlight: function() {},
  highlightOnScroll: true,
  highlightOffset: 100,
  anchorName: function(i, heading, prefix) {
    if(heading.id.length) {
      return heading.id;
    }
    var candidateId = heading.innerText.replace(/[^a-z0-9]/ig, ' ').replace(/\s+/g, '-').toLowerCase();
    if (verboseIdCache[candidateId]) {
      var j = 2;
      while(verboseIdCache[candidateId + j]) {
        j++;
      }
      candidateId = candidateId + '-' + j;
    }
    verboseIdCache[candidateId] = true;
    return prefix + '-' + candidateId;
  },
  headerText: function(i, heading) {
    return heading.innerText();
  },
  itemClass: function(i, heading, prefix) {
    return prefix + '-' + heading.tagName.toLowerCase();
  }

};
