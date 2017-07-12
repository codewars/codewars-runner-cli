import XCTest

public class CodewarsObserver: XCTestObservation {
  public func testBundleWillStart(_ testBundle: Bundle) {}

  public func testSuiteWillStart(_ testSuite: XCTestSuite) {
    let name = testSuite.name
    if name != "All tests" && name != "__current.xctest" {
      printAndFlush("\n<DESCRIBE::>\(name)")
    }
  }

  public func testCaseWillStart(_ testCase: XCTestCase) {
    var name = testCase.name
    if name.contains(".") {
      name = name.components(separatedBy: ".")[1]
    }
    printAndFlush("\n<IT::>\(name)")
  }

  public func testCase(_ testCase: XCTestCase, didFailWithDescription description: String, inFile filePath: String?, atLine lineNumber: UInt) {
    if testCase.testRun!.unexpectedExceptionCount > 0 {
      printAndFlush("\n<ERROR::>\(esc(description))")
    } else {
      printAndFlush("\n<FAILED::>\(esc(description))")
    }
  }

  public func testCaseDidFinish(_ testCase: XCTestCase) {
    let testRun = testCase.testRun!
    if testRun.hasSucceeded {
      printAndFlush("\n<PASSED::>Test Passed")
    }
    printAndFlush("\n<COMPLETEDIN::>\(formatMS(testRun.totalDuration))")
  }

  public func testSuiteDidFinish(_ testSuite: XCTestSuite) {
    let name = testSuite.name
    if name != "All tests" && name != "__current.xctest" {
      let testRun = testSuite.testRun!
      printAndFlush("\n<COMPLETEDIN::>\(formatMS(testRun.totalDuration))")
    }
  }

  public func testBundleDidFinish(_ testBundle: Bundle) {}

  fileprivate func printAndFlush(_ message: String) {
    print(message)
    fflush(stdout)
  }

  fileprivate func esc(_ str: String) -> String {
    return str.replacingOccurrences(of: "\n", with: "<:LF:>")
  }

  fileprivate func formatMS(_ t: TimeInterval) -> String {
    return String(round(1e4*1e3*t)/1e4)
  }
}
