/**
 CRUD interface for an emacs BBDB file. Contacts are given and returned
 in JSON form.

 Contacts are identified with firtname and last name and a hash ID,
 which is pulled out of the contact when fetch from the file.

 Relies heavily on an emacs lisp initialization file to provide the basic
 emacs interface.
 */
var EmacsBatcher = require('./emacs-batcher');

var phoneVector = function(telephone){
    return [{name: 'vector'}, telephone.label, telephone.number];
};

var addressVector = function(address){
    return [{name: 'vector'},
            address.label,
            [{name: 'quote'},address.address],
            address.city,
            address.state,
            address.postalCode,
            address.country];
};

var bbdbCreate = function(contact){
    var bbdbsexp = [{name: 'bbdb-create-internal'},
                    [{name: 'quote'}, {car: contact.firstName, cdr: contact.lastName}], [], [],
                    [{name: 'quote'}, contact.organization],
                    [{name: 'quote'}, contact.email],
                    [{name: 'list'}].concat(contact.telephone.map(phoneVector)),
                    [{name: 'list'}].concat(contact.address.map(addressVector)),
                    []
                   ];
    return bbdbsexp;
};

var setq = function(name, value){
    return [{name: 'setq'}, {name: name}, value];
};

ContactsStore = function(emacsCmd, bbdbFile, initFile){
    this.bbdbFile = bbdbFile;
    this.emacsBatcher = new EmacsBatcher(emacsCmd, initFile);
};

ContactsStore.prototype.fetchMatching = function(text, callback){
    var setqBbdbFileForm = setq("bbdb-file", this.bbdbFile);
    var bbdbSearch = [{name: "fetch-contacts-json-batch"}, text];
    this.emacsBatcher.run([setqBbdbFileForm, bbdbSearch], function(err,stdout,stderr){
        if (err){
            callback(err);
        } else {
            callback(undefined, JSON.parse(stdout));
        }
    });
};

ContactsStore.prototype.create = function(contact, callback){
    var setqBbdbFileForm = setq("bbdb-file", this.bbdbFile);
    var bbdbCreateForm = bbdbCreate(contact);
    var saveBuffersForm = [{name: 'save-some-buffers'}, {name: 't'}];
    this.emacsBatcher.run([setqBbdbFileForm, bbdbCreateForm, saveBuffersForm],
                          function(err,stdout,stderr){
        if (err) {
            callback(err);
        } else {
            callback(undefined);
        }
    });
};

ContactsStore.prototype.delete = function(firstName, lastName, hash, callback){
    var setqBbdbFileForm = setq("bbdb-file", this.bbdbFile);
    var bbdbDelete = [{name: "woe-delete-record"}, firstName, lastName, hash];
    this.emacsBatcher.run([setqBbdbFileForm, bbdbDelete],
                          function(err,stdout,stderr){
        if (err){
            callback(err);
        } else {
            callback(undefined);
        }
    });
};

ContactsStore.prototype.update = function(firstName, lastName, hash, contact, callback){
    var setqBbdbFileForm = setq("bbdb-file", this.bbdbFile);

    // weird that all this is necessary...
    var contactJsonStr = JSON.stringify(contact);
    contactJsonStr = JSON.stringify(contactJsonStr);
    contactJsonStr = contactJsonStr.substring(1, contactJsonStr.length-1);
    var bbdbUpdate = [{name: "woe-update-record-json"}, firstName, lastName, hash, contactJsonStr];
    this.emacsBatcher.run([setqBbdbFileForm, bbdbUpdate], function(err,stdout,stderr){
        if (err) {
            callback(err);
        } else {
            callback(undefined);
        }
    });
};

exports.ContactsStore = ContactsStore;
exports.bbdbCreate = bbdbCreate;
