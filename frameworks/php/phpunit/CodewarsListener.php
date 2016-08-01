<?php
    use PHPUnit\Framework\TestCase;

    class CodewarsListener extends PHPUnit_Util_Printer implements PHPUnit_Framework_TestListener
    {

        // Writes any console output to the terminal
        protected function writeOutput(PHPUnit_Framework_Test $test = null)
        {
            // take care of TestSuite producing error (e.g. by running into exception) as TestSuite doesn't have hasOutput
            if ($test !== null && method_exists($test, 'hasOutput') && $test->hasOutput()) {
                print($test->getActualOutput());
            }
        }

        public function addError(PHPUnit_Framework_Test $test, Exception $e, $time)
        {
            $this->writeOutput($test);
            $message = preg_replace('/\n/', '<:LF:>', self::exceptionToString($e));
            printf("\n<FAILED::>%s\n", $message);
        }

        public function addFailure(PHPUnit_Framework_Test $test, PHPUnit_Framework_AssertionFailedError $e, $time)
        {
            $this->writeOutput($test);
            $message = preg_replace('/\n/', '<:LF:>', self::exceptionToString($e));
            printf("\n<FAILED::>%s\n", $message);
        }

        public function addIncompleteTest(PHPUnit_Framework_Test $test, Exception $e, $time)
        {
            // see https://phpunit.de/manual/current/en/incomplete-and-skipped-tests.html
        }

        public function addRiskyTest(PHPUnit_Framework_Test $test, Exception $e, $time)
        {
            // see https://phpunit.de/manual/current/en/risky-tests.html
        }

        public function addSkippedTest(PHPUnit_Framework_Test $test, Exception $e, $time)
        {
            // see https://phpunit.de/manual/current/en/incomplete-and-skipped-tests.html
        }

        public function startTest(PHPUnit_Framework_Test $test)
        {
            printf("\n<IT::>%s\n", $test->getName());
        }

        public function endTest(PHPUnit_Framework_Test $test, $time)
        {
            if ($test !== null && method_exists($test, 'hasFailed') && !$test->hasFailed()) {
                $this->writeOutput($test);
                printf("\n<PASSED::>%s\n", $test->getName());
            }
            printf("\n<COMPLETEDIN::>%s\n", number_format($time * 1000, 2, '.', ''));
        }

        public function startTestSuite(PHPUnit_Framework_TestSuite $suite)
        {
            printf("\n<DESCRIBE::>%s\n", $suite->getName());
        }

        public function endTestSuite(PHPUnit_Framework_TestSuite $suite)
        {
            printf("\n<COMPLETEDIN::>\n");
        }


        private static function exceptionToString(Exception $e)
        {
            if ($e instanceof PHPUnit_Framework_SelfDescribing) {
                $buffer = $e->toString();

                if ($e instanceof PHPUnit_Framework_ExpectationFailedException && $e->getComparisonFailure()) {
                    $comparisonFailure = $e->getComparisonFailure();
                    $expectedString = $comparisonFailure->getExpectedAsString();
                    $actualString = $comparisonFailure->getActualAsString();
                    if($actualString || $expectedString) {
                        $buffer = $buffer . sprintf("\nExpected: %s\nActual  : %s", $expectedString, $actualString);
                    }
                }

                if (!empty($buffer)) {
                    $buffer = trim($buffer) . "\n";
                }
            } elseif ($e instanceof PHPUnit_Framework_Error) {
                $buffer = $e->getMessage() . "\n";
            } elseif ($e instanceof PHPUnit_Framework_ExceptionWrapper) {
                $buffer = $e->getClassname() . ': ' . $e->getMessage() . "\n";
            } else if(method_exists($e, 'getMessage')) {
                $buffer = get_class($e) . ': ' . $e->getMessage() . "\n";
            } else {
                $buffer = $e . "\n";
            }

            return $buffer;
        }
    }