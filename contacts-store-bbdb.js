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
    var bbdbSearch = [{name: "bbdb"}, text];
    var dumpBbdb = [{name: 'dump-buffer'}, "*BBDB*"];
    this.emacsBatcher.run([setqBbdbFileForm, bbdbSearch, dumpBbdb], callback);
};

ContactsStore.prototype.update = function(contact){
   
};

ContactsStore.prototype.create = function(contact, callback){
    var setqBbdbFileForm = setq("bbdb-file", this.bbdbFile);
    var bbdbCreateForm = bbdbCreate(contact);
    var saveBuffersForm = [{name: 'save-some-buffers'}, {name: 't'}];
    this.emacsBatcher.run([setqBbdbFileForm, bbdbCreateForm, saveBuffersForm ], callback);
};

exports.ContactsStore = ContactsStore;
exports.bbdbCreate = bbdbCreate;
