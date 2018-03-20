//
// Note: This example test is leveraging the Mocha test framework.
// Please refer to their documentation on https://mochajs.org/ for help.
//

// The module 'assert' provides assertion methods from node
import * as assert from 'assert';

// You can import and use all API from the 'vscode' module
// as well as import your extension to test it
import * as vscode from 'vscode';
import * as dotty from '../src/extension';

// Defines a Mocha test suite to group tests of similar kind together
suite("vscode-dotty tests", function () {

    // Defines a Mocha unit test
    test("dummy test", function() {
        assert.equal(1, 1)
    });
});
