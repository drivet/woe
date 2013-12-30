var app = require("../app");

describe("Hello world", function() {
    it("says hello", function() {
        expect(app.helloWorld()).toEqual("Hello world!");
    });
});
