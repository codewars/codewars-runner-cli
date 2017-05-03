return function(options)
  local busted = require 'busted'
  local handler = require 'busted.outputHandlers.base'()
  local function escape(s)
    return s:gsub('\n', '<:LF:>')
  end

  local function onDescribeStart(element, parent)
    print('\n<DESCRIBE::>' .. element.name)
    return nil, true
  end
  local function onDescribeEnd(element, parent)
    local s = '\n<COMPLETEDIN::>%.4f'
    print(s:format(1000*element.duration))
    return nil, true
  end

  local function onTestStart(element, parent)
    print('\n<IT::>' .. element.name)
    return nil, true
  end
  local function onTestEnd(element, parent, status, trace)
    if status == 'success' then
      print('\n<PASSED::>Test Passed')
    end
    local s = '\n<COMPLETEDIN::>%.4f'
    print(s:format(1000*element.duration))
    return nil, true
  end

  local function onTestFailure(element, parent, message, debug)
    print('\n<FAILED::>Test Failed')
    -- remove '/home/codewarrior/lua/fixture.lua:\d+: ' from message
    print('\n<LOG:ESC:>' .. escape(message:sub(message:find(' ') + 1)))
    return nil, true
  end

  local function onTestError(element, parent, message, debug)
    print('\n<ERROR::>Test Error')
    print('\n<LOG:ESC:Traceback>' .. escape(message))
    return nil, true
  end

  local function onError(element, parent, message, debug)
    if element.descriptor ~= 'it' then
      print('\n<ERROR::>Error')
      print('\n<LOG:ESC:>' .. escape(message))
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
