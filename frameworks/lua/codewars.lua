return function(options)
  local busted = require 'busted'
  local handler = require 'busted.outputHandlers.base'()
  local function escape(s)
    return s:gsub('\n', '<:LF:>')
  end

  local function onDescribeStart(element, parent)
    print('<DESCRIBE::>' .. element.name)
    return nil, true
  end
  local function onDescribeEnd(element, parent)
    local s = '<COMPLETEDIN::>%.4f'
    print(s:format(1000*element.duration))
    return nil, true
  end

  local function onTestStart(element, parent)
    print('<IT::>' .. element.name)
    return nil, true
  end
  local function onTestEnd(element, parent, status, trace)
    if status == 'success' then
      print('<PASSED::>Test Passed')
    end
    local s = '<COMPLETEDIN::>%.4f'
    print(s:format(1000*element.duration))
    return nil, true
  end

  local function onTestFailure(element, parent, message, debug)
    print('<FAILED::>Test Failed')
    -- remove '/home/codewarrior/lua/fixture.lua:\d+: ' from message
    print('<LOG:ESC:>' .. escape(message:sub(message:find(' ') + 1)))
    return nil, true
  end

  local function onTestError(element, parent, message, debug)
    print('<ERROR::>Test Error')
    print('<LOG:ESC:Traceback>' .. escape(message))
    return nil, true
  end

  local function onError(element, parent, message, debug)
    if element.descriptor ~= 'it' then
      print('<ERROR::>Error')
      print('<LOG:ESC:>' .. escape(message))
    end
    return nil, true
  end
  busted.subscribe({ 'describe', 'start' }, onDescribeStart, { predicate = handler.cancelOnPending })
  busted.subscribe({ 'describe', 'end' }, onDescribeEnd, { predicate = handler.cancelOnPending })
  busted.subscribe({ 'test', 'start' }, onTestStart, { predicate = handler.cancelOnPending })
  busted.subscribe({ 'test', 'end' }, onTestEnd, { predicate = handler.cancelOnPending })
  busted.subscribe({ 'error', 'it' }, onTestError)
  busted.subscribe({ 'failure', 'it' }, onTestFailure)
  busted.subscribe({ 'error' }, onError)
  return handler
end
