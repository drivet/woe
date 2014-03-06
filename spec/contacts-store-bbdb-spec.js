var storemod = require("../contacts-store-bbdb");
var fs = require("fs");

describe("ContactsStore", function() {
    var bbdbFile = "/tmp/bbdb.test";
    var contact = { "firstName":"Ricky",
                    "lastName": "Ricardo",
                    "organization": ["org1", "org2"],
                    "email": ["a@b.org", "b@h.org"],
                    "telephone": [{"label": "Home",
                                   "number": "6666666666"
                                  },
                                  {"label": "Cell",
                                   "number": "7777777777"
                                  }
                                 ],
                    "address": [{"label": "Home",
                                 "address": ["543 Blah de Blah"],
                                 "city": "Montreal",
                                 "state": "Quebec",
                                 "postalCode": "H4C2L2",
                                 "country": "Canada"
                                },
                                {"label": "Business",
                                 "address": ["777 Foo de Foo", "Suite 529"],
                                 "city": "Toronto",
                                 "state": "Ontario",
                                 "postalCode": "J4K2T3",
                                 "country": "Canada"
                                }
                               ]
                  };

    var createDone = false;

    afterEach(function(){
        if (fs.existsSync(bbdbFile)){
            fs.unlinkSync(bbdbFile);
        }
    });

    it("should generate a correct bbdb creation form", function() {
        var expectedForm =  [{name: 'bbdb-create-internal'},
                             [{name: 'quote'}, {car: "Ricky", cdr: "Ricardo"}], [], [],
                             [{name: 'quote'}, ["org1", "org2"]],
                             [{name: 'quote'}, ["a@b.org", "b@h.org"]],
                             [{name: 'list'}, [{name: 'vector'}, "Home", "6666666666"],
                                              [{name: 'vector'}, "Cell", "7777777777"]],
                             [{name: 'list'},
                              [{name: 'vector'}, "Home", [{name: 'quote'},["543 Blah de Blah"]],
                               "Montreal", "Quebec", "H4C2L2", "Canada"],
                              [{name: 'vector'}, "Business", [{name: 'quote'},["777 Foo de Foo", "Suite 529"]],
                               "Toronto", "Ontario", "J4K2T3", "Canada"]],
                             []];
        expect(storemod.bbdbCreate(contact)).toEqual(expectedForm);
    });
    
    it('should create a new contact', function(){
        runs(function(){
            createDone = false;
            store = new ContactsStore("/usr/local/bin/emacs", bbdbFile,
                                      "/home/dcr/node_projects/woe/emacs-scripts/woe.el");
            store.create(contact, function(error) {
                createDone = true;
            });
        });

        waitsFor(function(){
            return createDone;
        }, "Create should be done", 1000);
        
        runs(function() {
            expect(fs.readFileSync(bbdbFile).toString()).toContain(contact.firstName);
        });
    });

    it('should fetch existing contacts', function(){
        copyFile('/home/dcr/node_projects/woe/spec/bbdb.test', bbdbFile);
        var fetchedContactsJson = "";
        runs(function(){
            fetchDone = false;
            store = new ContactsStore("/usr/local/bin/emacs", bbdbFile,
                                      "/home/dcr/node_projects/woe/emacs-scripts/woe.el");
            store.fetchMatching("ricky", function(error, contacts) {
                fetchedContactsJson = contacts;
                fetchDone = true;
            });
        });

        waitsFor(function(){
            return fetchDone;
        }, "Fetch should be done", 1000);
        
        runs(function() {
            expect(fetchedContactsJson.length).toEqual(1);
        });
    });
    
    it('should delete a contact', function(){
        copyFile('/home/dcr/node_projects/woe/spec/bbdb.test', bbdbFile);
        //expect(fs.readFileSync(bbdbFile).toString()).toContain("Ricky");
        // ricky's hash is known in advance 
        var hash = "29fbf9cf1e016d37152f216bab01d924";
        
        runs(function(){
            deleteDone = false;
            store = new ContactsStore("/usr/local/bin/emacs", bbdbFile,
                                      "/home/dcr/node_projects/woe/emacs-scripts/woe.el");
            store.delete("Ricky", "Ricardo", hash, function(error) {
                deleteDone = true;
            });
        });

        waitsFor(function(){
            return deleteDone;
        }, "Delete should be done", 1000);
        
        runs(function() {
            expect(fs.readFileSync(bbdbFile).toString()).not.toContain("Ricky");
        });
    });
    
    it('should update a contacts email list', function(){
        copyFile('/home/dcr/node_projects/woe/spec/bbdb.test', bbdbFile);
        var hash = "29fbf9cf1e016d37152f216bab01d924";

        var contactUpdateJson = {
            "email": ["email1@email1.org", "email2@email2.org"]
        };
        
        runs(function(){
            updateDone = false;
            store = new ContactsStore("/usr/local/bin/emacs", bbdbFile,
                                      "/home/dcr/node_projects/woe/emacs-scripts/woe.el");
            store.update("Ricky", "Ricardo", hash,  contactUpdateJson, function(error) {
                updateDone = true;
            });
        });
        
        waitsFor(function(){
            return updateDone;
        }, "Update should be done", 1000);
        
        runs(function() {
            expect(fs.readFileSync(bbdbFile).toString()).toContain("email1@email1.org");
            expect(fs.readFileSync(bbdbFile).toString()).toContain("email2@email2.org");
        });
    });
    
    it('should update a contacts phone list', function(){
        copyFile('/home/dcr/node_projects/woe/spec/bbdb.test', bbdbFile);
        var hash = "29fbf9cf1e016d37152f216bab01d924";

        var contactUpdateJson = {
            "telephone": [{"label": "Home",
                           "areaCode": "514",
                           "prefix": "555",
                           "suffix": "6666",
                           "extension": "0"
                          },
                          {"label": "Cell",
                           "areaCode": "438",
                           "prefix": "444",
                           "suffix": "7777",
                           "extension": "0"
                          }
                         ]
        };
        
        runs(function(){
            updateDone = false;
            store = new ContactsStore("/usr/local/bin/emacs", bbdbFile,
                                      "/home/dcr/node_projects/woe/emacs-scripts/woe.el");
            store.update("Ricky", "Ricardo", hash,  contactUpdateJson, function(error) {
                updateDone = true;
            });
        });
        
        waitsFor(function(){
            return updateDone;
        }, "Update should be done", 1000);
        
        runs(function() {
            expect(fs.readFileSync(bbdbFile).toString()).toContain("514");
            expect(fs.readFileSync(bbdbFile).toString()).toContain("555");
            expect(fs.readFileSync(bbdbFile).toString()).toContain("6666");
            expect(fs.readFileSync(bbdbFile).toString()).toContain("438");
            expect(fs.readFileSync(bbdbFile).toString()).toContain("444");
            expect(fs.readFileSync(bbdbFile).toString()).toContain("7777");
        });
    });
});


function copyFile(source, target) {
    fs.createReadStream(source).pipe(fs.createWriteStream(target));
}

