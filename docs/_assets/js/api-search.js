/** This Webworker performs search on the API structure
 *
 *  It can be used as follows:
 *
 *  ```javascript
 *  var apiSearch = new Worker("<path to this file>");
 *  apiSearch.postMessage({
 *      "type": "setup",
 *      "search": "<search term>",
 *      "docs": <docs API>
 *  });
 *  ```
 *
 *  It posts a few different messages to its parent:
 *
 *  ```json
 *  {
 *      "type": "entityResult",
 *      "package": <parent package>,
 *      "entity": <entity>
 *  }
 *
 *  {
 *      "type": "memberResult",
 *      "package": <parent package>,
 *      "parent": <parent entity>,
 *      "member": <entity>
 *  }
 *  ```
 */
onmessage = function(e) {
    var docs = e.data.docs;
    var searchTerm = e.data.search;

    var regexForTerm = function(query) {
        var escaped = query.replace(/([\.\*\+\?\|\(\)\[\]\\])/g, '\\$1');
        if (query.toLowerCase() != query) {
            // Regexp that matches CamelCase subbits: "BiSe" is
            // "[a-z]*Bi[a-z]*Se" and matches "BitSet", "ABitSet", ...
            return new RegExp(escaped.replace(/([A-Z])/g,"[a-z]*$1"));
        }
        else { // if query is all lower case make a normal case insensitive search
            return new RegExp(escaped, "i");
        }
    };

    var searchRegex = regexForTerm(searchTerm);

    var filterPackages = function(entity) {
        switch(entity.kind) {
            case "val":
            case "def":
            case "type":
            case "package":
                return false;
            default:
                return true;
        }
    };

    // look at this higher order function, such syntax:
    var messageParentIfMatches = function(parent) {
        return function(entity) {
            var fullName = entity.path.join('.');

            if (searchRegex.test(fullName)) {
                postMessage({
                    "type": "entityResult",
                    "package": parent,
                    "entity": entity
                });
            }

            var searchChild = function(member) {
                if (searchRegex.test(member.name)) {
                    postMessage({
                        "type": "memberResult",
                        "package": parent,
                        "parent": entity,
                        "member": member,
                    });
                }
            };
            entity.members.forEach(searchChild);
        };
    };

    docs.forEach(function(pack) {
        pack.members
            .filter(filterPackages)
            .forEach(messageParentIfMatches(pack));
    });
}
