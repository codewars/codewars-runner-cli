# Environment

Code is executed within a Dockerized Ubuntu 14.04 container.

## Language

[brainf**k](http://esolangs.org/wiki/Brainfuck) (known as simply BF in this codebase)

## Interpreter

### BF

*["bf" fast Brainf**k Interpreter](http://manpages.ubuntu.com/manpages/trusty/man1/bf.1.html)*

Care has been taken to follow the standard implementation guidelines as outlined by [The Epistle to the Implementors](http://www.hevanet.com/cristofd/brainfuck/epistle.html) whenever possible.  Specifically:

- The newline `\n` and carriage return `\r` are treated like any other character by this interpreter in terms of both input and output; there is no special behavior configured in this case
- For the program input, when EOF is reached, note that a value of `-1` is stored in the cell under the pointer, which is considered a standard implementation, albeit not the most popular one.  Keep that in mind when writing your BF programs as programs that assume otherwise is likely to cause an infinite loop.
- Hacks are not enabled: `#` and `!` are treated like any other non-command character.
- The memory tape has been configured to contain exactly `30000` 8-bit cells as per the original implementation of Brainf\*\*k by Urban Müller (and therefore is **not** unbounded) and does not exhibit toroidal behavior - an out-of-bounds memory pointer is reported as an error
- Cell wrapping (`0 - 1 -> 255`, `255 + 1 -> 0`) is enabled as per the standard implementation

## Testing Framework

### BF

The [JavaScript CW-2 custom testing framework](https://www.codewars.com/docs/js-slash-coffeescript-test-reference) is used for testing brainf\*\*k code.  To execute the BF code with the desired input, simply make a call to the `runBF` function which optionally accepts 1 argument - the program input as a string.  For example, if you want the output of your CAT program with the input `"Codewars"`, simply do `runBF("Codewars")` in which case the string `"Codewars"` is returned provided your CAT program is correct.  If no input is required, simply call `runBF` without any arguments.

A basic example of testing a Hello World program:

```javascript
Test.describe("Your BF Hello World Program", function () {
  Test.it("should return the string \"Hello World!\"", function () {
    Test.assertEquals(runBF(), "Hello World!");
  });
});
```

And a basic example of testing a program that multiplies two numbers together:

```javascript
Test.describe("Your BF Multiply Program", function () {
  Test.it("should work for some fixed tests", function () {
    Test.assertEquals(runBF(String.fromCharCode(3, 5)), String.fromCharCode(15));
    Test.assertEquals(runBF(String.fromCharCode(9, 8)), "H"); // "H" has character code 72
    Test.assertEquals(runBF(" \n"), "@"); // Cell wrapping at work - (10 * 32) % 256 === 64
  });
  Test.it("should work for some random tests", function () {
    for (var i = 0; i < 100; i++) {
      var a = ~~(Math.random() * 128); // Random number from 0 to 127 (both inclusive)
      var b = ~~(Math.random() * 128);
      Test.assertEquals(runBF(String.fromCharCode(a, b)), String.fromCharCode((a * b) % 256)); // allowing for cell wrapping when product exceeds 255
    }
  });
});
```

*NOTE: Although the JavaScript CW-2 Framework contains many different methods, only the following should be needed in most cases to test BF output:*

1. `Test.describe`
2. `Test.it`
3. `Test.assert(Not)Equals`

`Test.expect` may also be used in special circumstances but it is recommended to avoid it unless absolutely necessary.
