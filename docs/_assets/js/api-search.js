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
onmessage = function({ data: { docs, search } }) {
    const regexForTerm = (query) => {
        const escaped = query.replace(/([\.\*\+\?\|\(\)\[\]\\])/g, '\\$1');
        return query.toLowerCase() !== query
            ? new RegExp(escaped.replace(/([A-Z])/g, "[a-z]*$1"))
            : new RegExp(escaped, "i");
    };

    const searchRegex = regexForTerm(search);

    const filterPackages = (entity) => !["val", "def", "type", "package"].includes(entity.kind);

    const messageParentIfMatches = (parent) => (entity) => {
        const fullName = entity.path.join('.');

        if (searchRegex.test(fullName)) {
            postMessage({
                type: "entityResult",
                package: parent,
                entity
            });
        }

        entity.members.forEach((member) => {
            if (searchRegex.test(member.name)) {
                postMessage({
                    type: "memberResult",
                    package: parent,
                    parent: entity,
                    member
                });
            }
        });
    };

    docs.forEach((pack) => {
        pack.members
            .filter(filterPackages)
            .forEach(messageParentIfMatches(pack));
    });
};