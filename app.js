var express = require('express');
var OrgStore = require("./orgstore").OrgStore;
var FileNotFoundError = require("./orgstore").FileNotFoundError;

var app = express();
var store = new OrgStore("/home/dcr/pim/wiki");

app.get('/:path?', function(req, res){
    store.fetch(req.params.path, function(err, data) {
        if(err){
            if (err instanceof FileNotFoundError) {
                res.send(404, "File not found");
            } else {
                res.send(500, "Unknown error");
            }
        } else {
            res.set('Content-Type', 'text/html');
            res.send(new Buffer(data.toString()));
        }
    });
});

app.listen(3000);
console.log('Listening on port 3000');
