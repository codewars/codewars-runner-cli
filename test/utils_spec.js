
var expect = require('chai').expect;
var parseBodyToArgs = require('../lib/util').parseBodyToArgs;


describe( 'parseBodyToArgs', function(){
    it( 'should parse body into command line args', function() {
        var body = {l: 'ruby', c: 'solution'}
        var args = parseBodyToArgs(body);

        expect(args ).to.contain('-l ruby');
        expect(args ).to.contain(' -c solution');
    });

    it( 'should parse body with mix shortcut/full name params', function() {
        var body = {l: 'ruby', solution: 'solution'};
        var args = parseBodyToArgs(body);

        expect(args ).to.contain('-l ruby');
        expect(args ).to.contain(' --solution solution');
    });
});