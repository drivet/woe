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
                             [{name: 'list'}, [{name: 'vector'}, "Home", [{name: 'quote'},["543 Blah de Blah"]],
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
                                      "/home/dcr/node_projects/woe/emacs-scripts/bbdb-init.el");
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
        
    });
    
});
