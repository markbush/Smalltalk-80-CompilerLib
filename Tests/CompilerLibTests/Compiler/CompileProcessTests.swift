import XCTest
@testable import CompilerLib

final class CompileProcessTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("Process", instanceVariables: ["nextLink", "suspendedContext", "priority", "myList"])
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

  func testSuspendedContext() throws {
    let source = """
suspendedContext: aContext
	suspendedContext _ aContext

"""
    // 3 .. 5
    let expected = [16, 97, 120]
    try runningSource(source, expecting: expected)
  }

  func testPriority() throws {
    let source = """
priority: anInteger
	"Set the receiver's priority to anInteger."

	anInteger<=Processor highestPriority
		ifTrue: [priority _ anInteger]
		ifFalse: [self error: 'priority too high']

"""
    // 11 .. 24
    let expected = [16, 67, 210, 180, 155, 16, 129, 2, 146, 112, 33, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }


  func testOffList() throws {
    let source = """
offList
	"Inform the receiver that it has been taken off a list that it was suspended
	on.  This is to break a backpointer."

	myList _ nil

"""
    // 3 .. 5
    let expected = [115, 99, 120]
    try runningSource(source, expecting: expected)
  }

  func testSuspend() throws {
    let source = """
suspend
	"Stop the process that the receiver represents in such a way that it can be
	restarted at a later time (by sending the receiver the message resume). If
	the receiver represents the activeProcess, suspend it. Otherwise fail and
	the code below will remove the receiver from the list of waiting
	processes. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 88>
	Processor activeProcess == self
		ifTrue: [self primitiveFailed]
		ifFalse:
			[Processor remove: self ifAbsent: [self error: 'This process was not active'].
			myList _ nil]

"""
    // 19 .. 45
    let expected = [65, 213, 112, 198, 155, 112, 212, 164, 16, 65, 112, 137, 117, 200, 164, 4, 112, 35, 226, 125, 240, 135, 115, 129, 3, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTerminate() throws {
    let source = """
terminate
	"Perhaps this method should be primitive.  If the a process might run at
	any moment (like a Delay), and another process is trying to terminate it,
	there is a race condition in this method.  The remove:ifAbsent: may get
	confused.  One way to avoid this is to let Delays terminate themselves.
	See MessageTally spyEvery:on: for an example. "

	| context |
	Processor activeProcess == self
		ifTrue:
			[thisContext sender == nil ifFalse: [thisContext sender release].
			thisContext removeSelf suspend]
		ifFalse:
			["remove me first, then destroy me"
			myList == nil
				ifFalse:
					[myList remove: self ifAbsent: [].
					myList _ nil].
			context _ suspendedContext.
			suspendedContext _ nil.
			(context ~~ nil and: [context sender ~~ nil])
				ifTrue: [context sender release]]

"""
    // 19 .. 79
    let expected = [71, 214, 112, 198, 172, 15, 137, 210, 115, 198, 168, 4, 137, 210, 209, 135, 137, 213, 212, 164, 38, 3, 115, 198, 168, 13, 3, 112, 137, 117, 200, 164, 2, 115, 125, 240, 135, 115, 99, 1, 104, 115, 97, 16, 115, 227, 156, 16, 210, 115, 227, 144, 114, 155, 16, 210, 209, 144, 115, 135, 120]
    try runningSource(source, expecting: expected)
  }



  func testInstall() throws {
    let source = """
install: aContext
	"Replace the suspendedContext with aContext."

	self == Processor activeProcess
		ifTrue: [^self error: 'The active process cannot install contexts'].
	suspendedContext _ aContext

"""
    // 11 .. 22
    let expected = [112, 67, 210, 198, 155, 112, 33, 224, 124, 16, 97, 120]
    try runningSource(source, expecting: expected)
  }

  func testResume() throws {
    let source = """
resume
	"Allow the process that the receiver represents to continue. Put the receiver
	in line to become the activeProcess. Fail if the receiver is already waiting
	in a queue (in a Semaphore or ProcessScheduler). Essential. See Object
	documentation whatIsAPrimitive."

	<primitive: 87>
	self primitiveFailed

"""
    // 9 .. 12
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testPopTo() throws {
    let source = """
popTo: aContext
	"Replace the suspendedContext with aContext, releasing all contexts
	between the currently suspendedContext and it."

	self == Processor activeProcess
		ifTrue: [^self error: 'The active process cannot pop contexts'].
	suspendedContext releaseTo: aContext.
	suspendedContext _ aContext

"""
    // 13 .. 28
    let expected = [112, 67, 210, 198, 155, 112, 33, 224, 124, 1, 16, 228, 135, 16, 97, 120]
    try runningSource(source, expecting: expected)
  }

// TODO: duplicates #printOn: in literals
//   func testPrintOn() throws {
//     let source = """
// printOn: aStream
// 	super printOn: aStream.
// 	aStream nextPutAll: ' in '.
// 	suspendedContext printOn: aStream
//
// """
//     // 13 .. 26
//     let expected = [112, 16, 133, 32, 135, 16, 34, 225, 135, 1, 16, 227, 135, 120]
//     try runningSource(source, expecting: expected)
//   }

}
