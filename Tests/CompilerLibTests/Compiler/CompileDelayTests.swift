import XCTest
@testable import CompilerLib

final class CompileDelayTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("Delay", instanceVariables: ["delayDuration", "resumptionTime", "delaySemaphore", "delayInProgress"])
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

  func testDisable() throws {
    let source = """
disable
	AccessProtect wait.
	delayInProgress ifTrue:
		[ActiveDelay == self
			ifTrue: [SuspendedDelays isEmpty
						ifTrue: [Processor signal: nil atTime: 0.
								ActiveDelay _ nil]
						ifFalse: [SuspendedDelays removeFirst activate]]
			ifFalse: [SuspendedDelays remove: self].
		delaySemaphore terminateProcess.
		delayInProgress _ false].
	AccessProtect signal

"""
    // 27 .. 67
    let expected = [65, 208, 135, 3, 172, 31, 72, 112, 198, 172, 17, 67, 217, 172, 9, 71, 115, 117, 246, 135, 115, 129, 200, 146, 67, 213, 212, 146, 67, 112, 226, 135, 2, 218, 135, 114, 99, 65, 219, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testSetResumption() throws {
    let source = """
setResumption
	delayInProgress
		ifTrue: [self error: 'This Delay is already waiting']
		ifFalse:
			[delayDuration == nil
				ifTrue: [resumptionTime == nil ifTrue: [self error: 'uninitialized Delay']]
				ifFalse: [resumptionTime _ Time millisecondClockValue + delayDuration].
			delayInProgress _ true]

"""
    // 13 .. 46
    let expected = [3, 156, 112, 36, 226, 164, 25, 0, 115, 198, 172, 10, 1, 115, 198, 155, 112, 35, 226, 144, 115, 149, 65, 208, 0, 176, 129, 1, 135, 113, 129, 3, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testPostSnapshot() throws {
    let source = """
postSnapshot
	| pendingDelay |
	delayInProgress
		ifTrue:
			[resumptionTime == nil
				ifTrue: [self error: 'uninitialized Delay']
				ifFalse: ["convert from milliseconds since Jan. 1 1901 to local millisecond clock"
						pendingDelay _ resumptionTime - (Time totalSeconds * 1000).
						pendingDelay _ pendingDelay max: 0.
						resumptionTime _ Time millisecondClockValue + pendingDelay]]

		"if false then this delay must be ready and waiting (on AccessProtect) to resume"

"""
    // 17 .. 47
    let expected = [3, 172, 27, 1, 115, 198, 156, 112, 38, 229, 164, 17, 1, 65, 208, 34, 184, 177, 104, 16, 117, 227, 104, 65, 212, 16, 176, 129, 1, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testPreSnapshot() throws {
    let source = """
preSnapshot
	| pendingDelay |
	delayInProgress
		ifTrue:
			[resumptionTime == nil
				ifTrue: [self error: 'uninitialized Delay']
				ifFalse: ["convert from local millisecond clock to milliseconds since Jan. 1 1901"
						pendingDelay _ resumptionTime - Time millisecondClockValue.
						resumptionTime _ Time totalSeconds * 1000 + pendingDelay]]
		ifFalse:
			[self error: 'This Delay is not waiting']

"""
    // 17 .. 47
    let expected = [3, 172, 23, 1, 115, 198, 156, 112, 38, 224, 164, 13, 1, 67, 210, 177, 104, 67, 212, 37, 184, 16, 176, 129, 1, 146, 112, 33, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testReactivate() throws {
    let source = """
reactivate
	"Make sure the timer is armed to go off for this delay."

	delayInProgress ifTrue:
		[TimingSemaphore initSignals.
		Processor signal: TimingSemaphore atTime: resumptionTime]

	"if false then the timer has already fired for this delay and it is waiting (on AccessProtect) to resume"

"""
    // 11 .. 21
    let expected = [3, 159, 65, 208, 135, 67, 65, 1, 242, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testResume() throws {
    let source = """
resume
	"The receiver's delay duration has expired, the process the receiver
	suspended will resume now. "

	Processor signal: nil atTime: 0.
	delayInProgress _ false.
	delaySemaphore signal

"""
    // 9 .. 19
    let expected = [65, 115, 117, 240, 135, 114, 99, 2, 210, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testDelayInProgress() throws {
    let source = """
delayInProgress: aBoolean

	delayInProgress _ aBoolean

"""
    // 3 .. 5
    let expected = [16, 99, 120]
    try runningSource(source, expecting: expected)
  }

  func testWait() throws {
    let source = """
wait
	"Suspend the active process for an amount of time specified when the receiver
	was initialized."

	AccessProtect wait.
	self setResumption.
	ActiveDelay == nil
		ifTrue: [self activate]
		ifFalse: [resumptionTime < ActiveDelay resumptionTime
				ifTrue:
					[SuspendedDelays add: ActiveDelay.
					self activate]
				ifFalse: [SuspendedDelays add: self]].
	AccessProtect signal.
	delaySemaphore wait

"""
    // 21 .. 57
    let expected = [65, 208, 135, 112, 210, 135, 69, 115, 198, 155, 112, 214, 164, 15, 1, 69, 215, 178, 158, 68, 69, 227, 135, 112, 214, 146, 68, 112, 227, 135, 65, 216, 135, 2, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testResumption() throws {
    let source = """
resumption: millisecondCount
	delayDuration _ nil.
	resumptionTime _ millisecondCount.
	delayInProgress _ false.
	delaySemaphore _ Semaphore new

"""
    // 5 .. 14
    let expected = [115, 96, 16, 97, 114, 99, 64, 204, 98, 120]
    try runningSource(source, expecting: expected)
  }

  func testDelay() throws {
    let source = """
delay: millisecondCount
	delayDuration _ millisecondCount.
	delayInProgress _ false.
	delaySemaphore _ Semaphore new

"""
    // 5 .. 12
    let expected = [16, 96, 114, 99, 64, 204, 98, 120]
    try runningSource(source, expecting: expected)
  }

  func testActivate() throws {
    let source = """
activate
	"For the moment, the receiver will be the next Delay to resume the
	reciever's suspended process."

	ActiveDelay _ self.
	TimingSemaphore initSignals.
	Processor signal: TimingSemaphore atTime: resumptionTime

"""
    // 13 .. 24
    let expected = [112, 130, 192, 66, 209, 135, 68, 66, 1, 243, 135, 120]
    try runningSource(source, expecting: expected)
  }


}
