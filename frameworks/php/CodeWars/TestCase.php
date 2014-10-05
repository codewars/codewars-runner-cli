<?php

namespace CodeWars;

use PHPUnit_Framework_TestCase;

class TestCase extends PHPUnit_Framework_TestCase {

    public $userScriptVariables = [];

    public function __construct($name = null, array $data = array(), $dataName = '') {
        $this->userScriptVariables = $data['user_vars'];
        unset($data['user_vars']);

        return parent::__construct($name, $data, $dataName);
    }
}
