
try
{
    // prevent anyone from peeking at the code we passed in
    if (global.process)
    {
        global.process.execArgv = null;
        global.process._eval = null;
    }

    var util = require('util');
    var deepEquals = require('lodash').isEqual;

    var fnToString = Function.toString;
    Function.prototype.toString = function ()
    {
        switch (this)
        {
            case Test.expect:
            case Test.randomNumber:
            case Test.randomize:
            case Test.randomToken:
                return '[Codewars Code]';

            default:
                return fnToString.call(this);

        }
    };

    var methodCalls = {},
        describing = false,
        correct = 0,
        incorrect = 0,
        failed = [],
        beforeCallbacks = [],
        afterCallbacks = [];

    $$_SUCCESS__ = null;
    $STDOUT = [];

    var _expect = function (passed, failMsg, options)
    {
        options = options || {};
        if (Object.__proto__.extraCredit || Object.prototype.extraCredit) throw 'extraCredit cannot be on the object prototype';

        if (passed)
        {
            var successMsg = "Test Passed";
            if (options.successMsg)
            {
                successMsg += ": " + options.successMsg;
            }
            console.log('<PASSED::>' + Test.format(successMsg));
            correct++;
        }
        else
        {
            failMsg = _message(failMsg) || 'Value is not what was expected';
            if (options.extraCredit)
            {
                failMsg = (options.extraCredit !== true) ? _message(options.extraCredit) : failMsg;
                failMsg = combineMessages(["Test Missed", failMsg], ": ");
                console.log("<MISSED::>" + Test.format(failMsg));
                incorrect++;
            }
            else
            {
                console.log("<FAILED::>" + Test.format(failMsg));
                var error = new Test.Error(failMsg);
                if (describing)
                {
                    failed.push(error);
                }
                else
                {
                    throw error;
                }
            }
        }
    }

//    console._log = console.log;
//    console.log = function(){
//        var out = [];
//        Array.prototype.slice.apply(arguments).forEach(function(arg){
//            out.push(Test.format(arg));
//        });
//
//        console._log(out.join(' '));
//    };

    function combineMessages(msgs, separator)
    {
        return msgs.filter(function (m){ return m != null;}).join(separator)
    }

    function _message(msg, prefix)
    {
        if (typeof msg == 'function')
        {
            msg = msg()
        }
        else if (typeof msg == 'array')
        {
            msg = combineMessages(msg, ' - ')
        }
        msg = prefix ? (prefix + ' - ' + msg) : msg;
        return msg || '';
    }

    var Test = {
        // we use this instead of util.inspect so that we can support the indent option and json options
        stringify: function(obj, indent)
        {
            var cache = [];
            return JSON.stringify(obj, function(key, value){
                if (typeof value === 'object' && value !== null) {
                    // Circular reference found, discard key
                    if (cache.indexOf(value) !== -1) return "[Circular]";
                }
                // Store value in our collection
                cache.push(value);
                return value;
            }, indent);
        },
        // formats an value to be outputted. If a function is provided then it will be evaluated,
        // if an object is provided then it will JSONfied. By default
        // any line breaks will be replaced with <:BR:> so that the entire message is considered
        // one group of data.
        format: function (obj, options)
        {
            options = options || {};
            var out = '';
            if (typeof obj == 'string')
            {
                out = obj;
            }
            else if (typeof obj == 'function')
            {
                out = obj.toString();
            }
            else
            {
                if (obj && obj !== true){

                    // for backwards compatibility we will support the indent option
                    if (options.indent || options.json)
                    {
                        out = Test.stringify(obj, options.indent ? 4 : 0)
                    }
                    else
                    {
                        out = util.inspect(obj, options);
                    }
                }
                else{
                    out = ('' + obj);
                }
            }
            // replace linebreaks with LF so that they can be converted back to line breaks later. Otherwise
            // the linebreak will be treated as a new data item.
            return out.replace(/\n/g, '<:LF:>');
        },
        inspect: function (obj)
        {
            if (typeof obj == 'string')
            {
                return obj;
            }
            else
            {
                return obj && obj !== true ? JSON.stringify(obj) : ('' + obj)
            }
        },
        describe: function (msg, fn)
        {
            var start = new Date();
            try
            {
                if (describing) throw "cannot call describe within another describe";
                describing = true;
                console.log("<DESCRIBE::>" + Test.format(_message(msg)));
                fn();
            }
            catch (ex)
            {
                Test.handleError(ex);
            }
            finally
            {
                var ms = new Date() - start;
                console.log("<COMPLETEDIN::>" + ms);
                describing = false;
                beforeCallbacks = [];
                afterCallbacks = [];

                if (failed.length > 0)
                {
                    throw failed[0];
                }
            }
        },
        it: function (msg, fn)
        {
            if (!describing) throw '"it" calls must be invoked within a parent "describe" context';

            console.log("<IT::>" + Test.format(_message(msg)));
            beforeCallbacks.forEach(function (cb)
            {
                cb();
            });

            var start = new Date();
            try
            {
                fn();
            }
            catch (ex)
            {
                Test.handleError(ex);
            }
            finally
            {
                var ms = new Date() - start;
                console.log("<COMPLETEDIN::>" + ms);

                afterCallbacks.forEach(function (cb)
                {
                    cb();
                });
            }

        },
        before: function (cb)
        {
            beforeCallbacks.push(cb);
        },
        after: function (cb)
        {
            afterCallbacks.push(cb);
        },
        // handles an error and writes the appropriate output. If a function is provided it will handle the error
        // if the function errors, and then rethrow the exception
        handleError: function (ex)
        {
            if (typeof ex == "function")
            {
                try
                {
                    ex();
                }
                catch(ex)
                {
                    this.handleError(ex);
                    throw ex;
                }
            }
            else if (ex.name == 'AssertionError')
            {
                this.fail(ex.message);
            }
            else if (ex.name != "TestError")
            {
                console.log("<ERROR::>" + this.format(_message(Test.trace(ex))));
            }
        },
        // clean up the stack trace of the exception so that it doesn't give confusing results.
        // Results would be confusing because the user submitted code is compiled into a script where
        // additional code is injected and line numbers will not match.
        trace: function (ex)
        {
            return (ex.stack || ex.toString() || '')
                .toString()
                // remove file names (ie: (/cli-runner/...))
                .replace(/\s\(.*\)/g, '')
                // remove at [eval] statements
                .replace(/(at)*( Object.)*\s*[(]?\[eval\].*(:\d*)*[)]?\n/g, '')
                // remove stack trace beyond the Module information
                .replace(/at Module[\w\s.:\d\n]*/g, '')
                // remove at Object.<anonymous>
                .replace(/\t*at Object.<\w*>\n/g, '')
                // handleError is used to wrap top level code, so lets remove it so that it doesn't
                // confuse users who won't understand why it is there.
                .replace('at Object.Test.handleError', '');
        },
        pass: function ()
        {
            _expect(true);
        },
        fail: function (message)
        {
            _expect(false, message);
        },
        expect: function (passed, message, options)
        {
            _expect(passed, message, options)
        },
        assertSimilar: function (actual, expected, msg, options)
        {
            this.assertEquals(this.inspect(actual), this.inspect(expected), msg, options)
        },
        assertNotSimilar: function (actual, expected, msg, options)
        {
            this.assertNotEquals(this.inspect(actual), this.inspect(expected), msg, options)
        },
        assertEquals: function (actual, expected, msg, options) {
            if (actual !== expected) {
                msg = _message('Expected: ' + Test.inspect(expected) + ', instead got: ' + Test.inspect(actual), msg);
                Test.expect(false, msg, options);
            }
            else {
                options = options || {};
                options.successMsg = options.successMsg || 'Value == ' + Test.inspect(expected);
                Test.expect(true, null, options);
            }
        },
        assertNotEquals: function (a, b, msg, options) {
            if (a === b) {
                msg = _message('Not Expected: ' + Test.inspect(a), msg);
                Test.expect(false, msg, options);
            }
            else {
                options = options || {};
                options.successMsg = options.successMsg || 'Value != ' + Test.inspect(b);
                Test.expect(true, null, options);
            }
        },
        assertDeepEquals: function (actual, expected, msg, options) {
            if (deepEquals(actual, expected)) {
                options = options || {};
                options.successMsg = options.successMsg || 'Value deep equals ' + Test.inspect(expected);
                Test.expect(true, null, options);
            }
            else {
                msg = _message('Expected: ' + Test.inspect(expected) + ', instead got: ' + Test.inspect(actual), msg);
                Test.expect(false, msg, options);
            }
        },
		    assertNotDeepEquals: function (actual, expected, msg, options) {
            if (!deepEquals(actual, expected)) {
                options = options || {};
                options.successMsg = options.successMsg || 'Value not deep equals ' + Test.inspect(expected);
                Test.expect(true, null, options);
            }
            else {
                msg = _message('Value should not deep equal ' + Test.inspect(actual), msg);
                Test.expect(false, msg, options);
            }
        },
        expectNoError: function (msg, fn)
        {
            if (!fn)
            {
                fn = msg;
                msg = 'Unexpected error was thrown';
            }

            try
            {
                fn();
                Test.expect(true)
            }
            catch (ex)
            {
                if (ex.name == 'TestError')
                {
                    throw ex;
                }
                else
                {
                    msg += ': ' + ex.toString()
                    Test.expect(false, msg)
                }
            }
        },
        expectError: function (msg, fn, options)
        {
            if (!fn)
            {
                fn = msg;
                msg = 'Expected an error to be thrown'
            }

            var passed = false;
            try
            {
                fn();
            }
            catch (ex)
            {
                console.log('<b>Expected error was thrown:</b> ' + ex.toString());
                passed = true
            }

            Test.expect(passed, msg, options)
        },
        randomNumber: function ()
        {
            return Math.round(Math.random() * 100)
        },
        randomToken: function ()
        {
            return Math.random().toString(36).substr(8)
        },
        randomize: function (array)
        {
            var arr = array.concat(), i = arr.length, j, x;
            while (i)
            {
                j = (Math.random() * i) | 0;
                x = arr[--i];
                arr[i] = arr[j];
                arr[j] = x;
            }
            return arr;
        },
        sample: function (array)
        {
            return array[~~(array.length * Math.random())]
        },
        escapeHtml: function (html) {
            return String(html)
                .replace(/&/g, '&amp;')
                .replace(/"/g, '&quot;')
                .replace(/'/g, '&#39;')
                .replace(/</g, '&lt;')
                .replace(/>/g, '&gt;');
        },
        Error: function (message)
        {
            this.name = "TestError";
            this.message = (message || "");
        }
    }

//    Test.Error.prototype = require('assert').AssertionError.prototype;
    Test.Error.prototype = Error.prototype;

    Object.freeze(Test);
    Object.defineProperty(global, 'Test', {
        writable: false,
        configurable: false,
        value: Test
    })
    Object.defineProperty(global, 'describe', {
        writable: false,
        value: Test.describe
    })
    Object.defineProperty(global, 'it', {
        writable: false,
        value: Test.it
    })
    Object.defineProperty(global, 'before', {
        writable: false,
        value: Test.before
    })
    Object.defineProperty(global, 'after', {
        writable: false,
        value: Test.after
    })


}catch(ex)
{
    throw "Failed to load core API methods";
}
