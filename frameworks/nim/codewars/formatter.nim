import strutils, times, unittest

proc escapeLF(s: string): string = s.replace("\n", "<:LF:>")

proc removePrefix(s: string): string =
  # fixture.nim(8,20): Check failed: sub(1, 1) == 0
  # -------------------
  # remove assertion location because it's not useful on Codewars
  return s[s.find(':')+2 .. s.high]

type CodewarsOutputFormatter* = ref object of OutputFormatter
  ## Custom output formatter for Codewars
  suiteStartTime: float
  testErrors: seq[string]
  testStartTime: float
  testStackTrace: string

proc newCodewarsOutputFormatter*(): CodewarsOutputFormatter =
  CodewarsOutputFormatter(
    suiteStartTime: 0.0,
    testErrors: @[],
    testStackTrace: "",
    testStartTime: 0.0
  )

method suiteStarted*(f: CodewarsOutputFormatter, suiteName: string) =
  echo("\n<DESCRIBE::>" & escapeLF(suiteName))
  f.suiteStartTime = epochTime()

method testStarted*(f: CodewarsOutputFormatter, testName: string) =
  echo("\n<IT::>" & escapeLF(testName))
  f.testErrors.setLen(0)
  f.testStackTrace.setLen(0)
  f.testStartTime = epochTime()

method failureOccurred*(f: CodewarsOutputFormatter, checkpoints: seq[string], stackTrace: string) =
  f.testErrors.add(checkpoints)
  if stackTrace != nil:
    f.testStackTrace = stackTrace

method testEnded*(f: CodewarsOutputFormatter, testResult: TestResult) =
  let ms = (epochTime() - f.testStartTime)*1000
  case testResult.status:
  of OK:
    echo("\n<PASSED::>Test Passed")
  of SKIPPED:
    echo("\n<LOG::>Test Skipped")
  of FAILED:
    # TODO: stack trace. borrowing JUnitOutputFormatter's for now
    let failureMsg = if f.testStackTrace.len > 0 and f.testErrors.len > 0:
                       f.testErrors[^1]
                     elif f.testErrors.len > 0:
                       f.testErrors[0]
                     else:
                       "Test Failed"
    var errs = ""
    if f.testErrors.len > 1:
      let startIdx = if f.testStackTrace.len > 0: 0 else: 1
      let endIdx   = if f.testStackTrace.len > 0: f.testErrors.len - 2 else: f.testErrors.len - 1
      for i in startIdx..endIdx:
        if errs.len > 0: errs.add("\n")
        errs.add(f.testErrors[i])

    if f.testStackTrace.len > 0:
      echo("\n<ERROR::>Test Error")
      if errs.len > 0:
        echo("\n<LOG:ESC:Error>" & escapeLF(removePrefix(failureMsg & "\n" & errs)))
      else:
        echo("\n<LOG:ESC:Error>" & escapeLF(removePrefix(failureMsg)))
      echo("\n<TAB::Traceback>" & escapeLF(f.testStackTrace))
    else:
      echo("\n<FAILED::>" & escapeLF(removePrefix(failureMsg)))
      if errs.len > 0: echo("\n<LOG:ESC:Failure>" & escapeLF(errs))
  echo("\n<COMPLETEDIN::>" & ms.formatFloat(ffDecimal, precision=4))

method suiteEnded*(f: CodewarsOutputFormatter) =
  let ms = (epochTime() - f.suiteStartTime)*1000
  echo("\n<COMPLETEDIN::>", ms.formatFloat(ffDecimal, precision=4))
