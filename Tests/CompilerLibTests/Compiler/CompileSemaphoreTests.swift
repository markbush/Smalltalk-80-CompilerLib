import XCTest
@testable import CompilerLib

final class CompileSemaphoreTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("Semaphore", instanceVariables: ["firstLink", "lastLink", "excessSignals"])
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

  func testInitSignals() throws {
    let source = """
initSignals
	"Consume any excess signals the receiver may have accumulated."

	excessSignals _ 0
"""
    // 3 .. 5
    let expected = [117, 98, 120]
    try runningSource(source, expecting: expected)
  }

  func testCritical() throws {
    let source = """
critical: mutuallyExcludedBlock
	"Evaluate mutuallyExcludedBlock only if the receiver is not currently in the
	process of running the critical: message.  If the receiver is, evaluate
	mutuallyExcludedBlock after the other critical: message is finished."

	| blockValue |
	self wait.
	blockValue _ mutuallyExcludedBlock value.
	self signal.
	^blockValue
"""
    // 7 .. 17
    let expected = [112, 208, 135, 16, 201, 105, 112, 209, 135, 17, 124]
    try runningSource(source, expecting: expected)
  }

  func testWait() throws {
    let source = """
wait
	"The active Process must receive a signal through the receiver before
	proceeding.  If no signal has been sent, the active Process will be suspended
	until one is sent.  Essential.  See
	Object whatIsAPrimitive."

	<primitive: 86>
	self primitiveFailed

	"excessSignals>0
		ifTrue: [excessSignals _ excessSignals-1]
		ifFalse: [self addLastLink: Processor activeProcess suspend]"
"""
    // 9 .. 12
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testSignal() throws {
    let source = """
signal
	"Send a signal through the receiver. If one or more processes have been
	suspended trying to receive a signal, allow the first one to proceed. If no
	process is waiting, remember the excess signal. Essential. See Object documentation
	whatIsAPrimitive. "

	<primitive: 85>
	self primitiveFailed

	"self isEmpty
		ifTrue: [excessSignals _ excessSignals+1]
		ifFalse: [Processor resume: self removeFirstLink]"
"""
    // 9 .. 12
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTerminateProcess() throws {
    let source = """
terminateProcess
	self isEmpty
		ifFalse: [self removeFirst terminate]
"""
    // 9 .. 17
    let expected = [112, 210, 168, 4, 112, 209, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }
}
