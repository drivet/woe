var EmacsBatcher = require("../emacs-batcher");
var child = require('child_process');
var sexp = require('sexpression');

describe('Emacs Batcher', function(){
    var emacsCmd = '/path/to/emacs';
    var initFile = '/path/to/the/init.el';
    var callback = function(){};

    beforeEach(function(){
        spyOn(child, 'exec');
    });
    
    it('should use the callback specified', function(){
        var emacsBatcher = new EmacsBatcher(emacsCmd, '');
        emacsBatcher.run([], callback);
        expect(child.exec).toHaveBeenCalledWith(jasmine.any(String), callback);
    });

    it('should use the emacs path specified', function(){
        var emacsBatcher = new EmacsBatcher(emacsCmd, '');
        emacsBatcher.run([], callback);
        expectEmacsBatch();

    });

    it('should include init file in command', function(){
        var emacsBatcher = new EmacsBatcher(emacsCmd, initFile);
        emacsBatcher.run([], callback);
        expectEmacsBatch('-l '+initFile);
    });

    it('should include emacs lisp forms in eval options', function(){
        var simpleSexp1 = [{name: 'save-some-buffers'}, {name:'t'}];
        var simpleSexp2 = [{name: 'call-something'}, {name:'stuff'}];
        var emacsBatcher = new EmacsBatcher(emacsCmd, '');
        emacsBatcher.run([simpleSexp1,simpleSexp2], callback);
        expectEmacsBatch('--eval="(save-some-buffers t)" --eval="(call-something stuff)"');
    });
    
    it('should escape the emacs lisp forms', function(){
        var simpleSexp = [{name:'setq'}, {name: 'bbdb-file'}, '/path/to/bbdbfile'];
        var emacsBatcher = new EmacsBatcher(emacsCmd, '');
        emacsBatcher.run([simpleSexp], callback);
        expectEmacsBatch('--eval="(setq bbdb-file \\"/path/to/bbdbfile\\")"');
    });
    
    var expectEmacsBatch = function(extraCmd){
        var fullEmacsCmd = emacsCmd+' -batch';
        if (extraCmd) {
            fullEmacsCmd += ' ' + extraCmd;
        } 
        expect(child.exec).toHaveBeenCalledWith(fullEmacsCmd, jasmine.any(Function));
    };
   
});

