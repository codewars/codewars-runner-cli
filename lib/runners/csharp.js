var shovel = require('../shovel'),
	tmp=require('temporary');

module.exports.run = function run(opts, cb){
    shovel.start(opts, cb, {
        solutionOnly: function() {
			if(opts.solutionFile && !opts.solution){
				// should fix this: mocha tests don't pass by opts.process, so you get a solutionFile without solution read
				return {name: 'csharp', 'args': [opts.solutionFile]};
			}
            var code = opts.solution;
            if (opts.setup) {
                code = opts.setup + '\n' + code;
            }
			// Allways use temp file even if code can be passed by arg. 
			var file = new tmp.File();
			opts.tempFiles = (opts.tempFiles || []).push(file.path);
			file.writeFileSync(code);
			return {name: 'csharp', 'args': [file.path]};
		}
    });
};

