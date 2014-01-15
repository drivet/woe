var child = require('child_process');
var sexp = require('sexpression');

EmacsBatcher = function(emacsBin, initFile){
    this.emacsBin = emacsBin;
    this.initFile = initFile;
};

EmacsBatcher.prototype.run = function(formList, callback) {
    var emacsArgs = [this.emacsBin, "-batch"];
    if (this.initFile) {
        emacsArgs.push("-l");
        emacsArgs.push(this.initFile);
    }

    if (formList) {
        for (var i=0; i < formList.length; i++) {
            var formAsString = sexp.stringify(formList[i]);
            emacsArgs.push("--eval=" + JSON.stringify(formAsString));
        }
    }
    var emacsCmd = emacsArgs.join(" ");
    child.exec(emacsCmd, callback);
};

module.exports = EmacsBatcher;
