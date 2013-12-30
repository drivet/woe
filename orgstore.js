var spawn = require('child_process').spawn;
var pathmod = require('path');
var fs = require("fs");


FileNotFoundError = function(message){
    this.message = message;
};


OrgStore = function(rootdir){
    this.rootdir = rootdir;
};

OrgStore.prototype.fetch = function(path, callback){
    var requested_uri = pathmod.join(this.rootdir, path);
    var dirname = pathmod.dirname(requested_uri);
    var filename = pathmod.join(dirname, pathmod.basename(requested_uri, '.html'))+".org";
    fs.exists(filename, function(exists){
        if (exists) {
            spawn("emacs", ["-no-site-file", "-batch", "--visit="+filename,
                            "--funcall", "org-export-as-html-batch"]);
            fs.readFile(requested_uri, callback);
        } else {
            callback(new FileNotFoundError("Could not find "+filename));
        }
    });
};
/*
OrgStore.prototype.update = function(path, callback){
    var requested_uri = pathmod.join(this.rootdir, path);
    var dirname = pathmod.dirname(requested_uri);
    var filename = pathmod.join(dirname, pathmod.basename(requested_uri, '.html'))+".org";
    
};
*/
exports.OrgStore = OrgStore;
exports.FileNotFoundError = FileNotFoundError;
