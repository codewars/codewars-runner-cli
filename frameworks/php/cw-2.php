<?php
try {
  class Test {
    /* Keeps track of the number of passes.  Cannot be modified externally. */
    protected $passes = 0;

    /* Keeps track of the number of fails.  Cannot be modified externally. */
    protected $fails = 0;

    /* Ensures a "describe" block is not nested in another.  Cannot be modified externally. */
    protected $describing = false;

    /* Class constant for random token generation */
    const token_chars = "abcdefghijklmnopqrstuvwxyz0123456789";

    /* Describes the tests to be executed.  Wraps the test output in <DESCRIBE::> and <COMPLETEDIN::> */
    public function describe($msg, $fn) {
      /* If describing message is empty or not defined, set a default message */
      if (!$msg) $msg = "The PHP code to be tested and executed";

      /* Start timing the script */
      $start = microtime(true);

      try {
        /* If the current describe block is nested in another describe block, halt the process before any testing occurs */
        if ($this->describing) throw new Exception("cannot call describe within another describe");

        /* Now that a describe block is being used, another describe block cannot be nested inside it */
        $this->describing = true;

        /* Format the "describe" block and run the tests */
        echo "\n<DESCRIBE::>$msg\n";
        $fn();
      } catch (Exception $e) {
        /* If an error exists, output it to the user */
        echo "\n<ERROR::>$e\n";
      } finally {
        /* Time the script and round to the nearest millisecond.  Output that to the user. */
        $dur = round((microtime(true) - $start) * 1000);
        echo "\n<COMPLETEDIN::>$dur\n";

        /* The describe block has ended.  Reset "describing" */
        $this->describing = false;

        /* If any tests failed inside the current "describe" block, throw an error to prevent further execution */
        if ($this->fails > 0) throw new Exception("Failed Tests");
      }
    }

    /* "It" blocks */
    public function it($msg, $fn) {
      /* If message is blank, set default message */
      if (!$msg) $msg = "should pass all tests below";

      /* "it" blocks must be nested within "describe" blocks */
      if (!$this->describing) throw new Exception("\"it\" blocks must be called within \"describe\" blocks");

      /* Format "it" block */
      echo "\n<IT::>$msg\n";
      try {
        $fn();
      } catch (Exception $e) {
        echo "\n<ERROR::>$e\n";
      } finally {
        /* Terminate current "it" block - almost forgot :o */
        echo "\n<COMPLETEDIN::>\n";
      }
    }

    /* Basic Test Method */
    public function expect($passed, $msg = "Value was not what was expected") {
      if ($passed) {
        $this->passes++;
        echo "\n<PASSED::>Test Passed\n";
      } else {
        $this->fails++;
        echo "\n<FAILED::>$msg\n";
        if (!$this->describing) throw new Exception("Failed Test");
      }
    }

    /* Assertion Methods */
    public function assert_equals($actual, $expected, $msg = "Actual value did not match expected") {
      $this->expect($actual === $expected, "$msg - Expected: " . $this->display($expected) . ", but instead got: " . $this->display($actual));
    }
    public function assert_not_equals($actual, $expected, $msg = "Incorrect value returned") {
      $this->expect($actual !== $expected, "$msg - Algorithm should not have returned: " . $this->display($expected));
    }
    public function assert_similar($actual, $expected, $msg = "Actual value did not match expected") {
      $this->expect($this->check_similar($actual, $expected), "$msg - Expected: " . $this->display($expected) . ", but instead got: " . $this->display($actual));
    }
    public function assert_not_similar($actual, $expected, $msg = "Incorrect value returned") {
      $this->expect(!$this->check_similar($actual, $expected), "$msg - Algorithm should not have returned: " . $this->display($expected));
    }

    /* Error-Handling Methods */
    public function expect_error($msg, $fn) {
      /* Default Message */
      if (!$msg) $msg = "Expected error was not thrown";
      $error_thrown = false;
      try {
        $fn();
      } catch (Exception $e) {
        $error_thrown = true;
      } finally {
        $this->expect($error_thrown, $msg);
      }
    }
    public function expect_no_error($msg, $fn) {
      /* Default Message */
      if (!$msg) $msg = "Unexpected error was thrown";
      $error_not_thrown = true;
      try {
        $fn();
      } catch (Exception $e) {
        $error_not_thrown = false;
      } finally {
        $this->expect($error_not_thrown, $msg);
      }
    }

    /* Random Output Methods */
    public function random_number() {
      return rand(0, 100);
    }
    public function random_token() {
      $length = rand(8, 10);
      $token = "";
      for ($i = 0; $i < $length; $i++) {
        $token .= str_split(Test::token_chars)[floor(lcg_value() * strlen(Test::token_chars))];
      }
      return $token;
    }

    /* Helper Methods (not accessible externally) */
    protected function check_similar($actual, $expected) {
      if (!is_array($expected) || !is_array($actual)) return $actual === $expected;
      foreach ($expected as $key => $value) {
        if (!$this->check_similar($actual[$key], $expected[$key])) return false;
      }
      foreach ($actual as $key => $value) {
        if (!$this->check_similar($actual[$key], $expected[$key])) return false;
      }
      return true;
    }
    protected function display($value) {
      if ($value === true) return "true";
      if ($value === false) return "false";
      if ($value === NULL) return "NULL";
      if (is_string($value)) return '"' . htmlspecialchars($value) . '"';
      if (!is_array($value)) return $value;
      if (count($value) === 0) return "array()";
      $result = "array(";
      $result .= "<div style='margin-left:20px'>";
      $result .= implode(",<br />", array_map(function ($key, $val) {
        return $this->display($key) . " => " . $this->display($val);
      }, array_keys($value), $value));
      $result .= "</div>";
      $result .= ")";
      return $result;
    }
  }
  $test = new Test;
} catch (Exception $e) {
  throw new Exception("Failed to load core API methods");
}
?>
