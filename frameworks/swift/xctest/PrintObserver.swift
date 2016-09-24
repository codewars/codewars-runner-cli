// This source file is part of the Swift.org open source project
//
// Copyright (c) 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//
//  PrintObserver.swift
//  Prints test progress to stdout.
//

#if os(Linux) || os(FreeBSD)
    import Foundation
#else
    import SwiftFoundation
#endif

/// Prints textual representations of each XCTestObservation event to stdout.
/// Mirrors the Apple XCTest output exactly.
internal class PrintObserver: XCTestObservation {
    func testBundleWillStart(_ testBundle: Bundle) {}

    func testSuiteWillStart(_ testSuite: XCTestSuite) {
        if testSuite.name != "All tests" && testSuite.name != "bin.xctest" {
          printAndFlush("<DESCRIBE::>\(testSuite.name)")
        }
    }

    func getMethodName(_ testCase: XCTestCase) -> String {
      if testCase.name.contains(".") {
        return testCase.name.components(separatedBy: ".")[1]
      }
      return testCase.name
    }

    func format(_ str: String) -> String {
        return str.replacingOccurrences(of: "\n", with: "<:LF:>")
    }

    func testCaseWillStart(_ testCase: XCTestCase) {
        printAndFlush("<IT::>\(getMethodName(testCase))")
    }

    func testCase(_ testCase: XCTestCase, didFailWithDescription description: String, inFile filePath: String?, atLine lineNumber: UInt) {
        printAndFlush("<ERROR::>\(getMethodName(testCase)) : \(format(description))")
    }

    func testCaseDidFinish(_ testCase: XCTestCase) {
        let testRun = testCase.testRun!
        if !testRun.hasSucceeded {
            printAndFlush("<FAILED::>\(getMethodName(testCase))")
        }
        printAndFlush("<COMPLETEDIN::>\(formatTimeInterval(testRun.totalDuration))")
    }

    func testSuiteDidFinish(_ testSuite: XCTestSuite) {
        // Do nothing.
    }

    func testBundleDidFinish(_ testBundle: Bundle) {}

    private lazy var dateFormatter: DateFormatter = {
        let formatter = DateFormatter()
        formatter.dateFormat = "HH:mm:ss.SSS"
        return formatter
    }()

    fileprivate func printAndFlush(_ message: String) {
        print(message)
        fflush(stdout)
    }

    private func formatTimeInterval(_ timeInterval: TimeInterval) -> String {
        return String(timeInterval * 1000.0)
    }
}

extension PrintObserver: XCTestInternalObservation {
    func testCase(_ testCase: XCTestCase, didMeasurePerformanceResults results: String, file: StaticString, line: UInt) {
        printAndFlush("<IT::>\(getMethodName(testCase))")
        printAndFlush("<COMPLETEDIN::>\(results)")
    }
}
