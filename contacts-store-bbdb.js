var spawn = require('child_process').spawn;
var exec = require('child_process').exec;

var startSexp = function(quote){
    var sexp = "";
    if (quote){
        sexp += "'";
    }
    sexp += "(";
    return sexp;
};

var makeSexp = function(list, quote){
    return startSexp(quote) + list.join(" ") + ")";
};

var makeDottedPair = function(first, second, quote){
    return startSexp(quote) + first + " . " + second + ")";
};

var quote = function(str){
    return "\"" + str + "\"";
};

var makeVector = function(list){
    return "[" + list.join(" ") + "]";
};

var makePhoneVector = function(telephone){
    return makeVector([telephone.label, telephone.number].map(quote));
};

var makeAddressVector = function(address){
    var addressList = [];
    addressList.push(quote(address.label));
    addressList.push(makeSexp(address.address.map(quote)));
    addressList.push(quote(address.city));
    addressList.push(quote(address.state));
    addressList.push(quote(address.postalCode));
    addressList.push(quote(address.country));
    return makeVector(addressList);
};

var makeBbdbCreateForm = function(contact){
    var sexp = makeSexp(["bbdb-create-internal",
                         makeDottedPair(quote(contact.firstName),
                                        quote(contact.lastName),
                                        true),
                         "nil", "nil",
                         makeSexp(contact.organization.map(quote), true),
                         makeSexp(contact.email.map(quote), true),
                         makeSexp(contact.telephone.map(makePhoneVector), true),
                         makeSexp(contact.address.map(makeAddressVector), true),
                         "nil"
                        ], false);
    return sexp;
};


ContactsStore = function(emacsCmd, bbdbFile, bbdbInitFile){
    this.emacsCmd = emacsCmd;
    this.bbdbFile = bbdbFile;
    this.bbdbInitFile = bbdbInitFile;
};

ContactsStore.prototype.fetch = function(){
    
};

ContactsStore.prototype.update = function(contact){
   
};

ContactsStore.prototype.create = function(contact, callback){
    var bbdbCreateForm = makeBbdbCreateForm(contact);
    var setqBbdbFile = "(setq bbdb-file \""+this.bbdbFile+"\")";

    var emacsArgs =  [this.emacsCmd, "-batch", "-l", this.bbdbInitFile,
                      "--eval="+JSON.stringify(setqBbdbFile),
                      "--eval="+JSON.stringify(bbdbCreateForm),
                      "--eval=\"(save-some-buffers t)\""];
    var emacsCmd = emacsArgs.join(" ");
    exec(emacsCmd, callback);
};

exports.ContactsStore = ContactsStore;
exports.makeBbdbCreateForm = makeBbdbCreateForm;
