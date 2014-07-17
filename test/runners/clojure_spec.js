var expect = require('chai').expect;
var runner = require('../../lib/runners/clojure');

describe('clojure runner', function () {
    describe('.run', function () {
        it('should handle basic code evaluation', function (done) {
            runner.run({
                language: 'clojure',
                solution: '(println "42")'
            }, function (buffer) {
                console.log(buffer.stderr);
                expect(buffer.stdout).to.equal('42\n');
                done();
            });
        });
        it('should handle running a namespace with imports', function (done) {
            runner.run({
                language: 'clojure',
                solution: [
                    '(ns foo (:require [clojure.edn :as edn]))',
                    '(print (get (edn/read-string "{:code \\"wars\\"}") :code))'
                ].join('\n')
            }, function (buffer) {
                console.log(buffer.stderr);
                expect(buffer.stdout).to.equal('wars');
                done();
            });
        });
        it('should handle setup code', function (done) {
            runner.run({
                language: 'clojure',
                solution: [
                    '(ns foo.solution-code (:require [foo.fighters]))',
                    '(foo.fighters/everlong)'
                ].join('\n'),
                setup: [
                    '(ns foo.fighters)',
                    '(defn everlong [] (print "Hello, I\'ve waited here for you"))'
                ].join('\n')
            }, function (buffer) {
                console.log(buffer.stderr);
                expect(buffer.stdout).to.equal('Hello, I\'ve waited here for you');
                done();
            });
        });
    });
    describe('codewars test framework (clojure.test)', function () {
        it('should be able to run a basic test', function (done) {
            runner.run({
                language: 'clojure',
                solution: '(ns empty.namespace)',
                fixture: [
                    '(ns clojure.test.example (:use clojure.test))',
                    '(deftest add-1-to-1 (testing "arithmetic works" (is (= 2 (+ 1 1)))))'
                ].join('\n')
            }, function (buffer) {
                console.log(buffer.stderr);
                expect(buffer.stdout).to.contain('<DESCRIBE::>add-1-to-1');
                expect(buffer.stdout).to.contain('<IT::>arithmetic works');
                expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
                expect(buffer.stdout).to.contain('<COMPLETEDIN::>');
                done();
            });
        });
        it('should be able to fail', function (done) {
            runner.run({
                language: 'clojure',
                solution: '(ns empty.namespace)',
                fixture: [
                    '(ns clojure.test.example (:use clojure.test))',
                    '(deftest sad-path (testing "won\'t work" (is (= 2 1) "bad math")))'
                ].join('\n')
            }, function (buffer) {
                console.log(buffer.stderr);
                expect(buffer.stdout).to.contain('<DESCRIBE::>sad-path');
                expect(buffer.stdout).to.contain('<IT::>won\'t work');
                expect(buffer.stdout).to.contain('<FAILED::>bad math');
                expect(buffer.stdout).to.contain('expected: (= 2 1)');
                expect(buffer.stdout).to.contain('actual: (not (= 2 1))');
                done();
            });
        });
        it('should print', function (done) {
            runner.run({
                language: 'clojure',
                solution: '(ns foo) (defn bar [] (print "yolo") 1)',
                fixture: [
                    '(ns clojure.test.example (:use clojure.test) (:require [foo]))',
                    '(deftest printing (testing "foo/bar" (is (= 1 (foo/bar)))))'
                ].join('\n')
            }, function (buffer) {
                console.log(buffer.stderr);
                expect(buffer.stdout).to.contain('<DESCRIBE::>printing');
                expect(buffer.stdout).to.contain('yolo<IT::>foo/bar');
                expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
                expect(buffer.stdout).to.contain('<COMPLETEDIN::>');
                done();
            });
        });
        it('should have an error when there\'s an exception', function (done) {
            runner.run({
                language: 'clojure',
                solution: '(ns foo)',
                fixture: [
                    '(ns clojure.test.example (:use clojure.test))',
                    '(deftest exception (testing "1 / 0" (is (= 1 (/ 1 0)))))'
                ].join('\n')
            }, function (buffer) {
                console.log(buffer.stderr);
                expect(buffer.stdout).to.contain('<DESCRIBE::>exception');
                expect(buffer.stdout).to.contain('<IT::>1 / 0');
                expect(buffer.stdout).to.contain('<ERROR::>');
                expect(buffer.stdout).to.contain('expected: (= 1 (/ 1 0))');
                expect(buffer.stdout).to.contain('actual: java.lang.ArithmeticException');
                done();
            });
        });
        it('should fail fast', function (done) {
            runner.run({
                language: 'clojure',
                solution: '(ns empty.namespace)',
                fixture: [
                    '(ns clojure.test.example (:use clojure.test))',
                    '(deftest fast-fail',
                    '  (testing "quit early" (is (= 2 1) "not true"))',
                    '  (testing "shouldn\'t happen" (is (= 3 1) "can\'t get here"))',
                ')'
                ].join('\n')
            }, function (buffer) {
                console.log(buffer.stderr);
                expect(buffer.stdout).to.contain('<DESCRIBE::>fast-fail');
                expect(buffer.stdout).to.contain('<IT::>quit early');
                expect(buffer.stdout).to.contain('<FAILED::>not true');
                expect(buffer.stdout).to.contain('expected: (= 2 1)');
                expect(buffer.stdout).to.contain('actual: (not (= 2 1))');
                expect(buffer.stdout).to.not.contain("shouldn't happen");
                expect(buffer.stdout).to.not.contain("can't get here");
                done();
            });
        });
    });
});
