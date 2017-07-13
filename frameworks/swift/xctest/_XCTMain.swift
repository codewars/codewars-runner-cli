@_exported import Foundation
import XCTest
import Glibc

public func _XCTMain(_ testCases: [XCTestCaseEntry]) -> Never {
  XCTestObservationCenter.shared().addTestObserver(CodewarsObserver())

  let root = XCTestSuite(name: "All tests")
  let current = XCTestSuite(name: "__current.xctest")
  root.addTest(current)
  testCases.map(TestCaseSuite.init).forEach(current.addTest)
  root.run()
  exit(root.testRun!.totalFailureCount == 0 ? 0 : 1)
}

fileprivate class TestCaseSuite: XCTestSuite {
  private let testCaseClass: XCTestCase.Type?

  init(testCaseEntry: XCTestCaseEntry) {
    let testCaseClass = testCaseEntry.testCaseClass
    self.testCaseClass = testCaseClass
    super.init(name: String(describing: testCaseClass))
    for (testName, testClosure) in testCaseEntry.allTests {
      addTest(testCaseClass.init(name: testName, testClosure: testClosure))
    }
  }

  override func setUp() {
    if let testCaseClass = testCaseClass {
      testCaseClass.setUp()
    }
  }
  override func tearDown() {
    if let testCaseClass = testCaseClass {
      testCaseClass.tearDown()
    }
  }
}
