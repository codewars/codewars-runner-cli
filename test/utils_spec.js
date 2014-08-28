
var expect = require('chai').expect;
var parseBodyToArgs = require('../lib/util').parseBodyToArgs;
var parseBodyToArray = require('../lib/util').parseBodyToArray;


describe( 'parseBodyToArgs', function(){
    it( 'should parse body into command line args', function() {
        var body = {l: 'ruby', c: 'solution'}
        var args = parseBodyToArgs(body);

        expect(args ).to.contain('-l "ruby"');
        expect(args ).to.contain(' -c "solution"');
    });

    it( 'should parse body with mix shortcut/full name params', function() {
        var body = {l: 'ruby', solution: 'solution "a"'};
        var args = parseBodyToArgs(body);

        expect(args ).to.contain('-l "ruby"');
        expect(args ).to.contain(' --solution "solution \\"a\\"');
    });

});

describe( 'parseBodyToArray', function(){
    it( 'should parse body into command line args array', function() {
        var body = {l: 'ruby', c: 'solution'}
        var args = parseBodyToArray(body);

        expect(args[0]).to.equal('-l')
        expect(args[1]).to.equal('ruby');
        expect(args[3]).to.equal('solution');
    });

    it( 'should parse body with mix shortcut/full name params', function() {
        var body = {l: 'ruby', solution: 'solution "a"'};
        var args = parseBodyToArray(body);

        expect(args[2]).to.equal('--solution');
    });

    it( 'should support ignore values', function() {
        var body = {l: 'ruby', solution: 'solution "a"'};
        var args = parseBodyToArray(body, ['solution']);

        expect(args.length).to.equal(2);
    });
});