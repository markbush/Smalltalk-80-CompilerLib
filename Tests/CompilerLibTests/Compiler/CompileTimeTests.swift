import XCTest
@testable import CompilerLib

final class CompileTimeTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("Time", instanceVariables: ["hours", "minutes", "seconds"])
    compiler = Compiler(forClass: classDescription)
  }
  override func tearDown() {
    compiler = nil
    super.tearDown()
  }
  func runningSource(_ source: String, expecting expected: [Int]) throws {
    compiler.compileMethod(source)
    let actual = compiler.context.bytecodes.map { bytecode in bytecode.rawValue }
    XCTAssertEqual(actual.count, expected.count, "Unexpected number of bytecodes")
    let count = min(actual.count, expected.count)
    for i in 0..<count {
      XCTAssertEqual(actual[i], expected[i], "Different bytecodes at position \(i)")
    }
  }


  func testEquals() throws {
    let source = """
= aTime
	"Answer whether aTime represents the same second as the receiver."

	self species = aTime species
		ifTrue: [^hours = aTime hours
					& (minutes = aTime minutes)
					& (seconds = aTime seconds)]
		ifFalse: [^false]

"""
    // 13 .. 35
    let expected = [112, 212, 16, 212, 182, 172, 15, 0, 16, 209, 182, 1, 16, 210, 182, 224, 2, 16, 211, 182, 224, 124, 122]
    try runningSource(source, expecting: expected)
  }


  func testAddTime() throws {
    let source = """
addTime: timeAmount
	"Answer a new Time that is timeAmount after the receiver.  timeAmount is an
	instance of Date or Time."

	^Time fromSeconds: self asSeconds + timeAmount asSeconds

"""
    // 9 .. 16
    let expected = [65, 112, 210, 16, 210, 176, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testSubtractTime() throws {
    let source = """
subtractTime: timeAmount
	"Answer a new Time that is timeAmount before the receiver.  timeAmount is an
	instance of Date or Time."

	^Time fromSeconds: self asSeconds - timeAmount asSeconds

"""
    // 9 .. 16
    let expected = [65, 112, 210, 16, 210, 177, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testAsSeconds() throws {
    let source = """
asSeconds
	"Answer the number of seconds since midnight of the receiver."

	^3600 * hours + (60 * minutes + seconds)

"""
    // 7 .. 16
    let expected = [32, 0, 184, 33, 1, 184, 2, 176, 176, 124]
    try runningSource(source, expecting: expected)
  }

  func testPrintOn() throws {
    let source = """
printOn: aStream
	"Format is h:mm:ss am/pm"

	hours > 12
		ifTrue: [hours - 12 printOn: aStream]
		ifFalse: [hours < 1
					ifTrue: [12 printOn: aStream]
					ifFalse: [hours printOn: aStream]].
	aStream nextPutAll: (minutes < 10
							ifTrue: [':0']
							ifFalse: [':']).
	minutes printOn: aStream.
	aStream nextPutAll: (seconds < 10
							ifTrue: [':0']
							ifFalse: [':']).
	seconds printOn: aStream.
	aStream nextPutAll: (hours < 12
							ifTrue: [' am']
							ifFalse: [' pm'])

"""
    // 19 .. 80
    let expected = [0, 33, 179, 158, 0, 33, 177, 16, 224, 164, 11, 0, 118, 178, 155, 33, 16, 224, 146, 0, 16, 224, 135, 16, 1, 37, 178, 153, 36, 144, 35, 226, 135, 1, 16, 224, 135, 16, 2, 37, 178, 153, 36, 144, 35, 226, 135, 2, 16, 224, 135, 16, 0, 33, 178, 153, 39, 144, 38, 226, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testHours() throws {
    let source = """
hours: anInteger
	hours _ anInteger

"""
    // 3 .. 5
    let expected = [16, 96, 120]
    try runningSource(source, expecting: expected)
  }


  func testLessThan() throws {
    let source = """
< aTime
	"Answer whether aTime is earlier than the receiver."

	hours ~= aTime hours ifTrue: [^hours < aTime hours].
	minutes ~= aTime minutes ifTrue: [^minutes < aTime minutes].
	^seconds < aTime seconds

"""
    // 9 .. 33
    let expected = [0, 16, 208, 183, 156, 0, 16, 208, 178, 124, 1, 16, 209, 183, 156, 1, 16, 209, 178, 124, 2, 16, 210, 178, 124]
    try runningSource(source, expecting: expected)
  }

  func testStoreOn() throws {
    let source = """
storeOn: aStream
	aStream nextPutAll: '(', self class name, ' readFromString: ';
		print: self printString;
		nextPut: $)

"""
    // 19 .. 38
    let expected = [16, 136, 34, 112, 199, 211, 225, 36, 225, 224, 135, 136, 112, 214, 229, 135, 39, 196, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testHash() throws {
    let source = """
hash
	"Hash must be redefined since = was redefined."
	^((hours hash bitShift: 3) bitXor: minutes) bitXor: seconds

"""
    // 9 .. 17
    let expected = [0, 209, 34, 188, 1, 224, 2, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testHoursMinutesSeconds() throws {
    let source = """
hours: hourInteger minutes: minInteger seconds: secInteger
	hours _ hourInteger.
	minutes _ minInteger.
	seconds _ secInteger

"""
    // 3 .. 9
    let expected = [16, 96, 17, 97, 18, 98, 120]
    try runningSource(source, expecting: expected)
  }

}
