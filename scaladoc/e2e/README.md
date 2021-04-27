## Scaladoc e2e test suite

### Prerequisites

- install Node.js
- run `npm i`

### Running tests

- generate the test docs: `sbt scaladoc/generateTestcasesDocumentation`
- run the web server in the `output` directory: `python3 -m http.server 8080`
- run `npm run cypress:open` to see the cypress UI or `npm run cypress:run` to run tests heedlessly
