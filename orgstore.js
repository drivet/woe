var spawn = require('child_process').spawn;
var pathmod = require('path');
var fs = require("fs");


FileNotFoundError = function(message){
    this.message = message;
};


OrgStore = function(rootdir, extension){
    this.rootdir = rootdir;
    this.extension = extension;
};

OrgStore.prototype.fetchAndConvert = function(path, outputType, readCallback){
    var requestedPath = pathmod.join(this.rootdir, path) + "." + this.extension;
    var convertedPath = pathmod.join(this.rootdir, path) + "." + outputType;
    fs.exists(requestedPath, function(exists){
        if (exists) {
            var orgFunc = "org-export-as-" + outputType + "-batch";
            spawn("emacs", ["-no-site-file",
                            "-batch",
                            "--visit=" + requestedPath,
                            "--funcall", orgFunc]);
            fs.readFile(convertedPath, readCallback);
        } else {
            readCallback(new FileNotFoundError("Could not find "+requestedPath));
        }
    });
};

OrgStore.prototype.fetch = function(path, readCallback){
    var requestedPath = pathmod.join(this.rootdir, path) + "." + this.extension;
    fs.exists(requestedPath, function(exists){
        if (exists) {
            fs.readFile(requestedPath, readCallback);
        } else {
            readCallback(new FileNotFoundError("Could not find "+requestedPath));
        }
    });
};

OrgStore.prototype.update = function(path, contents, writeCallback){
    var requestedPath = pathmod.join(this.rootdir, path) + "." + this.extension;
    fs.exists(requestedPath, function(exists){
        if (exists) {
            fs.writeFile(requestedPath, contents, writeCallback);
        } else {
            writeCallback(new FileNotFoundError("Could not find "+requestedPath));
        }
    });
};

exports.OrgStore = OrgStore;
exports.FileNotFoundError = FileNotFoundError;
