var express = require('express');
var pathmod = require('path');
var OrgStore = require("./orgstore").OrgStore;
var FileNotFoundError = require("./orgstore").FileNotFoundError;

var app = express();
var store = new OrgStore("/home/dcr/pim/wiki", "org");

app.use(express.bodyParser());
app.use(express.methodOverride());
app.set('views', __dirname + '/views');
app.set('view engine', 'jade');

app.get(/^(.*)\/edit$/, function(req, res){
    var path_to_edit = req.params[0];
    store.fetch(path_to_edit, function(err, data) {
        if(err){
            if (err instanceof FileNotFoundError) {
                res.send(404, "File not found");
            } else {
                res.send(500, "Unknown error");
            }
        } else {
            res.render('edit', {
                title: "Editing " + path_to_edit,
                path_to_edit: path_to_edit,
                data: data
            });
        }
    });
});

app.get(/^(.*?)(?:\.([^.]*))?$/, function(req, res){
    var fullname = req.params[0];
    var ext = req.params[1] || 'html';
    
    store.fetchAndConvert(fullname, ext, function(err, data) {
        if(err){
            if (err instanceof FileNotFoundError) {
                res.send(404, "File not found");
            } else {
                res.send(500, "Unknown error");
            }
        } else {
            res.set('Content-Type', 'text/'+ext);
            res.send(new Buffer(data.toString()));
        }
    });
});

app.put(/^(.*)$/, function(req, res){
    store.update(req.url, req.param('data'), function(err) {
        if(err){
            if (err instanceof FileNotFoundError) {
                res.send(404, "File not found");
            } else {
                res.send(500, "Unknown error");
            }
        } else {
            res.redirect(req.url);
        }
    });
});

app.listen(3000);
console.log('Listening on port 3000');
