
// Codewars test module for Chapel
// Code based on Python, PHP and JS cw-2 modules

module Test {

  use Math;

  // Basic Test Method
  proc expect(passed: bool, msg: string = "Value was not what was expected",
              allow_raise: bool = false) throws {
    if passed {
      writeln("\n<PASSED::>Test Passed");
    } else {
      writeln("\n<FAILED::>" + msg);
      if allow_raise {throw new Error(msg);}
    }
  }

  // Assertion Methods
  proc assertEquals(actual: ?t_act, expected: ?t_exp, msg: string = "",
                    allow_raise: bool = true) : void {
    var message: string = msg;
    var equals_msg: string = actual + " should equal " + expected;
    if message == "" {
      message = equals_msg;
    } else {
      message += ": " + equals_msg;
    }
    expect(actual == expected, message, allow_raise);
  }

  proc assertNotEquals(actual: ?t_act, expected: ?t_exp, msg: string = "",
                       allow_raise: bool = true) : void {
    var message: string = msg;
    var equals_msg: string = actual + " should equal " + expected;
    if message == "" {
      message = equals_msg;
    } else {
      message += ": " + equals_msg;
    }
    expect(actual != expected, message, allow_raise);
  }

  proc assertFuzzyEquals(actual : real, expected: real, msg: string = "",
                         allow_raise: bool = true) : void {
    var in_range: bool = false;
    const max_err: real = 1e-9;

    if expected == 0 {
      in_range = abs(actual) <= max_err;
    } else {
      in_range = abs((actual - expected) / expected) <= max_err;
    }

    var message: string = msg;
    var equals_msg: string = "%.9r".format(actual) + " should equal "
                           + "%.9r".format(expected);
    if message == "" {
      message = equals_msg;
    } else {
      message += ": " + equals_msg;
    }
    expect(in_range, message, allow_raise);
  }

    // TODO: Error-Handling Methods
    // Currently not supported?

  /*proc expectError(msg, function) {
    var passed: bool = false;
    try {
      function();
    } catch {
      passed = true;
    }
    expect(passed, msg);
  }

  proc expectNoError(msg, function) {
    var passed: bool = true;
    try {
      function();
    } catch {
      passed = false;
    }
    expect(passed, msg);
  }*/

  proc _format(msg) {
    return msg.replace("\n", "<:LF:>");
  }

  // Describes the tests to be executed.
  proc describe(msg) {
    write("<DESCRIBE::>" + _format(msg));
  }

  // "It" blocks
  proc it(msg) {
    write("<IT::>" + _format(msg));
  }
}

use Test;
