<?php

namespace CodeWars;

use PHPUnit_Framework_TestCase;

/**
 * TestCase
 *
 */
class TestCase extends PHPUnit_Framework_TestCase {

    /**
     * An array of all the variable defined in the users script.
     * These will be extracted to be available within the test method.
     *
     * @var array
     */
    protected $userScriptVariables = [];

    /**
     * __construct
     *
     * Overwriting in order to fetch the user script variables.
     *
     * @param string $name
     * @param array  $data
     * @param string $dataName
     * @return void
     */
    public function __construct($name = null, array $data = array(), $dataName = '') {
        $this->userScriptVariables = $data['user_vars'];
        unset($data['user_vars']);

        return parent::__construct($name, $data, $dataName);
    }
}