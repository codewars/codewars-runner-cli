<?php

namespace Codewars;

use PHPUnit_Framework_TestCase;

class TestCase extends PHPUnit_Framework_TestCase
{

    public function getCwResults()
    {
        print_r($this->getResult());Die;
        return json_encode([
            'success' => !$this->hasFailed(),
            'status' => $this->getStatus(),
            'output' => $this->getResult()
        ]);
    }
}
