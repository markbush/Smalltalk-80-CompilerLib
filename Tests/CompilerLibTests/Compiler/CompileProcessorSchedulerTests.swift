import XCTest
@testable import CompilerLib

final class CompileProcessorSchedulerTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("ProcessorScheduler", instanceVariables: ["quiescentProcessLists", "activeProcess"])
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

  func testTimingPriority() throws {
    let source = """
timingPriority
	"Answer the priority at which the system processes keeping track of
	real time should run."

	^TimingPriority
"""
    // 5 .. 6
    let expected = [64, 124]
    try runningSource(source, expecting: expected)
  }

  func testHighestPriority() throws {
    let source = """
highestPriority
	"Answer the number of priority levels currently available for use."

	^quiescentProcessLists size
"""
    // 3 .. 5
    let expected = [0, 194, 124]
    try runningSource(source, expecting: expected)
  }

  func testLowIOPriority() throws {
    let source = """
lowIOPriority
	"Answer the priority at which most input/output processes should run.
	Examples are the process handling input from the user (keyboard,
	pointing device, etc.) and the process distributing input from a network."

	^LowIOPriority
"""
    // 5 .. 6
    let expected = [64, 124]
    try runningSource(source, expecting: expected)
  }

  func testUserSchedulingPriority() throws {
    let source = """
userSchedulingPriority
	"Answer the priority at which the window scheduler should run."

	^UserSchedulingPriority
"""
    // 5 .. 6
    let expected = [64, 124]
    try runningSource(source, expecting: expected)
  }

  func testHighIOPriority() throws {
    let source = """
highIOPriority
	"Answer the priority at which the most time critical input/output
	processes should run.  An example is the process handling input from a
	network."

	^HighIOPriority
"""
    // 5 .. 6
    let expected = [64, 124]
    try runningSource(source, expecting: expected)
  }

  func testSuspendFirstAt() throws {
    let source = """
suspendFirstAt: aPriority
	"Suspend the first Process that is waiting to run with priority aPriority."

	^self suspendFirstAt: aPriority
		  ifNone: [self error: 'No Process to suspend']
"""
    // 9 .. 21
    let expected = [112, 16, 137, 117, 200, 164, 4, 112, 34, 225, 125, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testSignalAtMilliseconds() throws {
    let source = """
signal: aSemaphore atMilliseconds: milliseconds
	"Signal the semaphore when the millisecond clock reaches the value of
	the second argument.  The second argument is a byte indexable object at
	least four bytes long (a 32-bit unsigned number with the low order
	8-bits stored in the byte with the lowest index).  Fail if the first
	argument is neither a Semaphore nor nil.  Essential.  See Object
	documentation whatIsAPrimitive."

	<primitive: 100>
	self primitiveFailed
"""
    // 9 .. 12
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testSetHighestPriority() throws {
    let source = """
highestPriority: newHighestPriority
	"Change the number of priority levels currently available for use."

	| continue newProcessLists |
	(quiescentProcessLists size > newHighestPriority
		and: [self anyProcessesAbove: newHighestPriority])
			ifTrue: [self error: 'There are processes with priority higher than '
													,newHighestPriority printString].
	newProcessLists _ Array new: newHighestPriority.
	1 to: ((quiescentProcessLists size) min: (newProcessLists size)) do:
		[:priority | newProcessLists at: priority put: (quiescentProcessLists at: priority)].
	quiescentProcessLists size to: newProcessLists size do:
		[:priority | newProcessLists at: priority put: LinkedList new].
	quiescentProcessLists become: newProcessLists
"""
    // 23 .. 88
    let expected = [0, 194, 16, 179, 155, 112, 16, 228, 144, 114, 158, 112, 34, 16, 211, 225, 224, 135, 69, 16, 205, 106, 118, 0, 194, 18, 194, 231, 137, 118, 200, 164, 8, 107, 18, 19, 0, 19, 192, 193, 125, 246, 135, 0, 194, 18, 194, 137, 118, 200, 164, 7, 107, 18, 19, 72, 204, 193, 125, 246, 135, 0, 18, 233, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testUserBackgroundPriority() throws {
    let source = """
userBackgroundPriority
	"Answer the priority at which user background processes should run."

	^UserBackgroundPriority
"""
    // 5 .. 6
    let expected = [64, 124]
    try runningSource(source, expecting: expected)
  }

  func testSuspendFirstAtIfNone() throws {
    let source = """
suspendFirstAt: aPriority ifNone: noneBlock
	"Suspend the first Process that is waiting to run with priority aPriority.
	If no Process is waiting, evaluate noneBlock"

	| aList |
	aList _ quiescentProcessLists at: aPriority.
	aList isEmpty
		ifTrue: [^noneBlock value]
		ifFalse: [^aList first suspend]
"""
    // 9 .. 22
    let expected = [0, 16, 192, 106, 18, 210, 154, 17, 201, 124, 18, 209, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testAnyProcessesAbove() throws {
    let source = """
anyProcessesAbove: highestPriority
	"Do any instances of Process exist with higher priorities?"

	^(Process allInstances select: [:aProcess | aProcess priority > highestPriority]) isEmpty
"""
    // 13 .. 28
    let expected = [67, 210, 137, 118, 200, 164, 6, 105, 17, 212, 16, 179, 125, 225, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testSystemBackgroundPriority() throws {
    let source = """
systemBackgroundPriority
	"Answer the priority at which system background processes should
	run.  Examples are an incremental garbage collector or status checker."

	^SystemBackgroundPriority
"""
    // 5 .. 6
    let expected = [64, 124]
    try runningSource(source, expecting: expected)
  }


  func testUserInterruptPriority() throws {
    let source = """
userInterruptPriority
	"Answer the priority at which user processes desiring immediate
	service should run.  Processes run at this level will preempt the window
	scheduler and should, therefore, not consume the processor forever."

	^UserInterruptPriority
"""
    // 5 .. 6
    let expected = [64, 124]
    try runningSource(source, expecting: expected)
  }

  func testYield() throws {
    let source = """
yield
	"Give other Processes at the current priority a chance to run"

	| semaphore |
	semaphore _ Semaphore new.
	[semaphore signal] fork.
	semaphore wait
"""
    // 11 .. 27
    let expected = [64, 204, 104, 137, 117, 200, 164, 3, 16, 210, 125, 209, 135, 16, 211, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTerminateActive() throws {
    let source = """
terminateActive
	"Terminate the process that is currently running."

	activeProcess terminate
"""
    // 5 .. 8
    let expected = [1, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testSignalAtTime() throws {
    let source = """
signal: aSemaphore atTime: timeInterval
	"Signal aSemaphore when the system's millisecond clock reaches
	timeInterval (an Integer)"

	| milliseconds |
	(timeInterval digitLength > 4 or: [timeInterval negative])
		ifTrue: [self error: 'Can''t convert time to double word'].
	milliseconds _ ByteArray new: 4.
	milliseconds at: 1 put: (timeInterval digitAt: 1).
	milliseconds at: 2 put: (timeInterval digitAt: 2).
	milliseconds at: 3 put: (timeInterval digitAt: 3).
	milliseconds at: 4 put: (timeInterval digitAt: 4).
	^self signal: aSemaphore atMilliseconds: milliseconds
"""
    // 21 .. 71
    let expected = [17, 211, 36, 179, 153, 113, 145, 17, 210, 155, 112, 33, 224, 135, 69, 36, 205, 106, 18, 118, 17, 118, 230, 193, 135, 18, 119, 17, 119, 230, 193, 135, 18, 39, 17, 39, 230, 193, 135, 18, 36, 17, 36, 230, 193, 135, 112, 16, 18, 248, 124]
    try runningSource(source, expecting: expected)
  }

  func testRemoveIfAbsent() throws {
    let source = """
remove: aProcess ifAbsent: aBlock
	"Remove aProcess from the list on which it is waiting for the processor. If
	it is not waiting, evaluate aBlock."

	(quiescentProcessLists at: aProcess priority)
		remove: aProcess ifAbsent: aBlock.
	^aProcess
"""
    // 7 .. 16
    let expected = [0, 16, 209, 192, 16, 17, 240, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testActivePriority() throws {
    let source = """
activePriority
	"Answer the priority level of the currently running Process."

	^activeProcess priority
"""
    // 5 .. 7
    let expected = [1, 208, 124]
    try runningSource(source, expecting: expected)
  }
}
