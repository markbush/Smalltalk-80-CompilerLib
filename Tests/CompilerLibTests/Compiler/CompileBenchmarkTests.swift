import XCTest
@testable import CompilerLib

final class CompileBenchmarkTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("Benchmark", instanceVariables: ["dummy", "verboseTranscript", "reporting", "reportStream", "fromList"])
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

  func testClearOutputs() throws {
    let source = """
clearOutputs
	"This allows you to get rid of your old accumulated output streams."

	Outputs _ nil

	"Benchmark new clearOutputs"

"""
    // 5 .. 8
    let expected = [115, 130, 192, 120]
    try runningSource(source, expecting: expected)
  }

  func testCloseOutput() throws {
    let source = """
closeOutput: aStream
	self streamsRatherThanFiles
		ifTrue: [aStream reset]
		ifFalse: [aStream close]

"""
    // 9 .. 18
    let expected = [112, 210, 154, 16, 209, 145, 16, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testSetOutputParameters() throws {
    let source = """
setOutputParameters
	BinaryChoice
		message: 'Would you like the transcript to just show labels, rather than full reports?'
		displayAt: Sensor mousePoint
		centered: true
		ifTrue: [verboseTranscript _ false]
		ifFalse: [verboseTranscript _ true].
	BinaryChoice
		message: 'Should the full reports to be output (saved on a file or stream)?'
		displayAt: Sensor mousePoint
		centered: true
		ifTrue: [reporting _ true]
		ifFalse: [reporting _ false].
	reporting ifTrue:
		[FillInTheBlank
			request: 'Please supply desired output name'
			displayAt: Sensor mousePoint
			centered: true
			action: [:answer | reportStream _ self makeOutputNamed: answer]
			initialAnswer: '.timing']

"""
    // 25 .. 101
    let expected = [65, 34, 68, 211, 113, 137, 117, 200, 164, 4, 114, 129, 1, 125, 137, 117, 200, 164, 4, 113, 129, 1, 125, 131, 160, 135, 65, 37, 68, 211, 113, 137, 117, 200, 164, 4, 113, 129, 2, 125, 137, 117, 200, 164, 4, 114, 129, 2, 125, 131, 160, 135, 2, 172, 21, 71, 40, 68, 211, 113, 137, 118, 200, 164, 7, 104, 112, 16, 233, 129, 3, 125, 42, 131, 166, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testOutputNamed() throws {
    let source = """
outputNamed: name
	"Answer with the output file or stream of the given name."

	self streamsRatherThanFiles
		ifTrue: [^Outputs at: name]
		ifFalse: [^Disk file: name]

"""
    // 11 .. 21
    let expected = [112, 211, 155, 66, 16, 192, 124, 65, 16, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testStringForDisplay() throws {
    let source = """
stringForDisplay
	^'testTextDisplay
	| clipRect para range scanner |
	clipRect _ Display boundingBox.
	para _ Paragraph withText: self textForDisplay.
	range _ 1 to: para numberOfLines.
	scanner _ DisplayScanner new.
	self test: [scanner
				displayLines: range
				in: para
				clippedBy: clipRect]
		labeled: ''displaying text'' repeated: 10

	"Benchmark new testTextDisplay"'

"""
    // 5 .. 6
    let expected = [32, 124]
    try runningSource(source, expecting: expected)
  }

  func testTextForDisplay() throws {
    let source = """
textForDisplay
	^self stringForDisplay asText

"""
    // 7 .. 10
    let expected = [112, 209, 208, 124]
    try runningSource(source, expecting: expected)
  }

  func testTimeRepeated10K() throws {
    let source = """
time: aBlock repeated10K: tenKTimes
	| i emptyBlock emptyTime blockTime |
	tenKTimes > 10000 ifTrue: [^self error: 'one hundred million repetitions is my limit'].
	emptyBlock _ [].
	emptyTime _
			Time millisecondsToRun:
					[1 to: tenKTimes do:
						[:j |
						i _ 0.
						[(i _ i + 1) <= 10000]
							whileTrue: [emptyBlock value]]].
	blockTime _
			Time millisecondsToRun:
					[1 to: tenKTimes do:
						[:j |
						i _ 0.
						[(i _ i + 1) <= 10000]
							whileTrue: [aBlock value]]].
	^blockTime - emptyTime

"""
    // 15 .. 104
    let expected = [17, 34, 179, 155, 112, 33, 224, 124, 137, 117, 200, 164, 2, 115, 125, 107, 68, 137, 117, 200, 164, 27, 118, 17, 137, 118, 200, 164, 18, 110, 117, 106, 18, 118, 176, 129, 66, 34, 180, 156, 19, 201, 135, 163, 243, 115, 125, 245, 125, 227, 108, 68, 137, 117, 200, 164, 27, 118, 17, 137, 118, 200, 164, 18, 110, 117, 106, 18, 118, 176, 129, 66, 34, 180, 156, 16, 201, 135, 163, 243, 115, 125, 245, 125, 227, 109, 21, 20, 177, 124]
    try runningSource(source, expecting: expected)
  }

  func testDisplayOutputNamed() throws {
    let source = """
displayOutputNamed: name
	"Create a window on the display to view the contents of the named output.
	Does not allow editing of the output file or stream."

	| output contents |
	output _ self outputNamed: name.
	contents _ self streamsRatherThanFiles
					ifTrue: [output contents]
					ifFalse: [output contentsOfEntireFile].
	StringHolderView
		open: (StringHolder new contents: contents)
		label: name

"""
    // 19 .. 40
    let expected = [112, 16, 224, 105, 112, 211, 154, 17, 210, 145, 17, 209, 106, 69, 71, 204, 18, 230, 16, 244, 135, 120]
    try runningSource(source, expecting: expected)
  }

// TODO: Adds literals in a different order!
//   func testMakeOutputNamed() throws {
//     let source = """
// makeOutputNamed: name
// 	"Create a new output file or stream of the given name."
//
// 	self streamsRatherThanFiles
// 		ifTrue: [Outputs == nil ifTrue: [Outputs _ Dictionary new].
// 				Outputs at: name put:
// 					(ReadWriteStream on: (String new: 1000))].
// 	^self outputNamed: name
//
// """
//     // 19 .. 43
//     let expected = [112, 214, 172, 17, 65, 115, 198, 155, 64, 204, 130, 193, 65, 16, 67, 68, 37, 205, 226, 193, 135, 112, 16, 231, 124]
//     try runningSource(source, expecting: expected)
//   }

// TODO: This saves the space ($ ) twice in the literal list (maybe due to the assignments?)
//   func testReadOutputIntoDictAndCollection() throws {
//     let source = """
// readOutput: aStream intoDict: aDict andCollection: aColl
// 	"Parse the output file or stream, aStream, putting labels and times in aDict.
// 	If aColl is non-nil (but rather an ordered collection), also add the labels to it
// 	in order."
//
// 	| leftBracket rightBracket tab space label value |
// 	leftBracket _ $[.
// 	rightBracket _ $].
// 	tab _ $	.
// 	space _ $ .
// 	aStream upTo: leftBracket.
// 	[aStream atEnd] whileFalse:
// 		[label _ aStream upTo: rightBracket.
// 		 aColl notNil ifTrue: [aColl add: label].
// 		 aStream next: 2; upTo: tab.
// 		 value _ Number readFrom: (ReadStream on: (aStream upTo: space)).
// 		 aDict at: label put: value.
// 		 aStream upTo: leftBracket].
// 	self closeOutput: aStream
//
// """
//     // 29 .. 88
//     let expected = [32, 107, 33, 108, 34, 109, 35, 110, 16, 19, 228, 135, 16, 197, 168, 39, 16, 20, 228, 111, 18, 214, 155, 18, 23, 229, 135, 16, 136, 119, 231, 135, 21, 228, 135, 73, 75, 16, 22, 228, 234, 232, 130, 72, 17, 23, 24, 193, 135, 16, 19, 228, 135, 163, 213, 112, 16, 236, 135, 120]
//     try runningSource(source, expecting: expected)
//   }

  func testStreamsRatherThanFiles() throws {
    let source = """
streamsRatherThanFiles
	"Should the named outputs be (internal Smalltalk-80) streams rather than disk files?
	They should if there is no file system.
	Feel free to change this method if you have a file system but want streams anyway."

	^Disk == nil

"""
    // 5 .. 8
    let expected = [64, 115, 198, 124]
    try runningSource(source, expecting: expected)
  }

  func testLongishString() throws {
    let source = """
longishString
	^ 'Now is the time for all good people to come to the aid of the cause of world peace.  It is just fine, even desirable, to love your country, if that means wanting it to play a beneficial role in the course of world events and be the best possible example of a good society.  But if it means wanting dominion over the rest of the world, it is not love but defensiveness or self-glorification, and will lead only to oblivion.'

"""
    // 5 .. 6
    let expected = [32, 124]
    try runningSource(source, expecting: expected)
  }

  func testFileOutputParameters() throws {
    let source = """
fileOutputParameters: aFileStream
	reporting _ true.
	reportStream _ aFileStream.
	verboseTranscript _ false

"""
    // 3 .. 9
    let expected = [113, 98, 16, 99, 114, 97, 120]
    try runningSource(source, expecting: expected)
  }

  func testSampleBitBLT() throws {
    let source = """
sampleBitBLT
	^BitBlt
		destForm: Display
		sourceForm: nil
		halftoneForm: Form black
		combinationRule: Form reverse
		destOrigin: 0@0
		sourceOrigin: 0@0
		extent: 400@400
		clipRect: Display boundingBox

"""
    // 19 .. 40
    let expected = [65, 66, 115, 68, 211, 68, 213, 117, 117, 187, 117, 117, 187, 38, 38, 187, 66, 215, 132, 8, 0, 124]
    try runningSource(source, expecting: expected)
  }

  func testRecur() throws {
    let source = """
recur: exp
	"Invokes a recursion involving ((2 raisedTo: exp+1) - 1) activations and
	returns."

	exp = 0 ifTrue: [^self].
	self recur: exp - 1.
	^self recur: exp - 1

"""
    // 5 .. 21
    let expected = [16, 117, 182, 152, 120, 112, 16, 118, 177, 224, 135, 112, 16, 118, 177, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testTestLabeledRepeated() throws {
    let source = """
test: aBlock labeled: label repeated: nTimes
	"This is the main message to a Benchmark. aBlock is repeated nTimes, and the
	results are reported."

	| time |
	time _ self time: aBlock repeated: nTimes.
	self
		report: label
		timedAt: time
		repeated: nTimes

"""
    // 7 .. 19
    let expected = [112, 16, 18, 240, 107, 112, 17, 19, 18, 131, 97, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTimeRepeated() throws {
    let source = """
time: aBlock repeated: nTimes
	"Answer how many milliseconds it takes to repeat aBlock nTimes, corrected for the
	time to repeat an empty block."

	| i emptyBlock emptyTime blockTime iBox |
	nTimes > 10000 ifTrue: [^self time: aBlock repeated10K: nTimes // 10000].
	emptyBlock _ [].
	emptyTime _ Time millisecondsToRun:
							[i _ 0.
							 [(i _ i + 1) <= nTimes] whileTrue: [emptyBlock value]].
	blockTime _ Time millisecondsToRun:
							[i _ 0.
							 [(i _ i + 1) <= nTimes] whileTrue: [aBlock value]].
	^blockTime - emptyTime

"""
    // 11 .. 83
    let expected = [17, 33, 179, 158, 112, 16, 17, 33, 189, 240, 124, 137, 117, 200, 164, 2, 115, 125, 107, 67, 137, 117, 200, 164, 17, 117, 106, 18, 118, 176, 129, 66, 17, 180, 156, 19, 201, 135, 163, 243, 115, 125, 226, 108, 67, 137, 117, 200, 164, 17, 117, 106, 18, 118, 176, 129, 66, 17, 180, 156, 16, 201, 135, 163, 243, 115, 125, 226, 109, 21, 20, 177, 124]
    try runningSource(source, expecting: expected)
  }

  func testCompareOldTimesToNewTimesOutputTo() throws {
    let source = """
compareOldTimes: oldName toNewTimes: newName outputTo: outName
	"Compare two sets of benchmark ouput reports."

	| oldDict newDict labels compareStream oldTime newTime |
	oldDict _ Dictionary new. newDict _ Dictionary new. labels _ OrderedCollection new.
	self readOutput: (self outputNamed: oldName) intoDict: oldDict andCollection: nil.
	self readOutput: (self outputNamed: newName) intoDict: newDict andCollection: labels.
	compareStream _ self makeOutputNamed: outName.
	labels do:
		[:label | (oldDict includesKey: label) ifTrue:
			[compareStream nextPutAll: label; cr; tab;
				nextPutAll: 'old time: ', (oldTime _ oldDict at: label) printString; tab;
				nextPutAll: 'new time: ', (newTime _ newDict at: label) printString; tab;
				nextPutAll: 'percent change: ', (newTime - oldTime * 100.0 / oldTime) printString; cr; cr]].
	self closeOutput: compareStream

	"Benchmark new
		compareOldTimes: 'test1.timing'
		toNewTimes: 'test2.timing'
		outputTo: 'compare1-2.timing'"

	"Benchmark new displayOutputNamed: 'compare1-2.timing'"

"""
    // 35 .. 144
    let expected = [64, 204, 107, 64, 204, 108, 65, 204, 109, 112, 112, 16, 227, 19, 115, 131, 98, 135, 112, 112, 17, 227, 20, 21, 131, 98, 135, 112, 18, 228, 110, 21, 137, 118, 200, 164, 66, 130, 73, 19, 25, 238, 172, 57, 22, 136, 25, 229, 135, 136, 214, 135, 136, 215, 135, 136, 41, 19, 25, 192, 129, 71, 218, 232, 229, 135, 136, 215, 135, 136, 43, 20, 25, 192, 129, 72, 218, 232, 229, 135, 136, 215, 135, 136, 44, 24, 23, 177, 45, 184, 23, 185, 218, 232, 229, 135, 136, 214, 135, 214, 144, 115, 125, 203, 135, 112, 22, 239, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testReportTimedAtRepeated() throws {
    let source = """
report: label timedAt: time repeated: numberOfTimes
	"Do all the reporting, both in the Transcript and on the output file or stream."

	| reportString |
	reportString _ self reportStringFor: label
						timedAt: time
						repeated: numberOfTimes.
	verboseTranscript
			ifTrue: [Transcript show: reportString]
			ifFalse: [Transcript show: '
[', label, ']'].
	reporting ifTrue:
		[reportStream nextPutAll: reportString.
		 fromList ifFalse: [self closeOutput: reportStream]]

"""
    // 19 .. 54
    let expected = [112, 16, 17, 18, 131, 96, 107, 1, 155, 66, 19, 225, 150, 66, 36, 16, 227, 37, 227, 225, 135, 2, 172, 11, 3, 19, 230, 135, 4, 168, 4, 112, 3, 231, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testReportStringForTimedAtRepeated() throws {
    let source = """
reportStringFor: label timedAt: time repeated: numberOfTimes
	"Generate the parsible string to represent the measurement of a benchmark."

	| nTimes unitTime seconds aStream|
	aStream _ WriteStream on: (String new: 200).
	nTimes _ numberOfTimes <= 1000
				ifTrue: [numberOfTimes]
				ifFalse: [numberOfTimes // 1000 * 1000].
	seconds _ time asFloat / 1000.
	aStream cr; nextPutAll: 'Testing:  [' , label , ']'; cr.
	aStream tab; print: nTimes; nextPutAll: ' repetition(s) in'; cr;
		tab; print: seconds; nextPutAll: ' seconds'; cr.
	nTimes ~= 1
		ifTrue:
			[unitTime _ (time * 1000) asFloat / nTimes.
			aStream tab; print: unitTime;
				nextPutAll: ' microseconds per repetition'; cr].
	^aStream contents

"""
    // 37 .. 131
    let expected = [65, 66, 35, 205, 224, 110, 18, 36, 180, 153, 18, 148, 18, 36, 189, 36, 184, 107, 17, 213, 36, 185, 109, 22, 136, 214, 135, 136, 41, 16, 232, 42, 232, 231, 135, 214, 135, 22, 136, 219, 135, 136, 19, 236, 135, 136, 45, 231, 135, 136, 214, 135, 136, 219, 135, 136, 21, 236, 135, 136, 46, 231, 135, 214, 135, 19, 118, 183, 172, 21, 17, 36, 184, 213, 19, 185, 108, 22, 136, 219, 135, 136, 20, 236, 135, 136, 47, 231, 135, 214, 135, 22, 131, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testFavoriteStringHolderView() throws {
    let source = """
favoriteStringHolderView
	| aStringHolderView message |
	message _ self longishString.
	aStringHolderView _ StringHolderView container: (StringHolder new contents: message).
	aStringHolderView window: (0@0 extent: 300@200).
	aStringHolderView translateBy: 100@250.
	aStringHolderView display.
	^aStringHolderView

"""
    // 29 .. 59
    let expected = [112, 208, 105, 66, 68, 204, 17, 227, 225, 104, 16, 117, 117, 187, 39, 40, 187, 230, 229, 135, 16, 42, 43, 187, 233, 135, 16, 220, 135, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testTestList() throws {
    let source = """
testList: selectorList
	"This message allows you to test a series of benchmarks and record all the results on
	a file or stream."

	fromList _ true.
	self setOutputParameters.
	selectorList do:
		[:selector | self perform: selector.
		 verboseTranscript ifTrue:
			[Transcript show: 'press any mouse button to continue'.
			 Sensor waitButton.
			 Transcript clear; refresh]].
	reporting ifTrue: [self closeOutput: reportStream].
	fromList _ false.

	"Benchmark new testList: #(testLoadInstVar testLoadLiteralIndirect testLoadLiteralNRef testLoadQuickConstant testLoadTempNRef)"

"""
    // 23 .. 67
    let expected = [113, 100, 112, 208, 135, 16, 137, 118, 200, 164, 23, 105, 112, 17, 225, 135, 1, 172, 13, 67, 36, 226, 135, 70, 213, 135, 67, 136, 215, 135, 216, 144, 115, 125, 203, 135, 2, 155, 112, 3, 233, 135, 114, 100, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestLoadInstVar() throws {
    let source = """
testLoadInstVar
	dummy _ 1.
	self test: [dummy == dummy. dummy == dummy.
			  dummy == dummy. dummy == dummy.
			  dummy == dummy. dummy == dummy.
			  dummy == dummy. dummy == dummy.
			  dummy == dummy. dummy == dummy. nil]
		labeled: 'load an instance variable, 20 times' repeated: 10000

	"Benchmark new testLoadInstVar"

"""
    // 9 .. 64
    let expected = [118, 96, 112, 137, 117, 200, 164, 42, 0, 0, 198, 135, 0, 0, 198, 135, 0, 0, 198, 135, 0, 0, 198, 135, 0, 0, 198, 135, 0, 0, 198, 135, 0, 0, 198, 135, 0, 0, 198, 135, 0, 0, 198, 135, 0, 0, 198, 135, 115, 125, 33, 34, 131, 96, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestLoadTempNRef() throws {
    let source = """
testLoadTempNRef
	| temp |
	temp _ 1.
	self test: [temp == temp. temp == temp.
			  temp == temp. temp == temp.
			  temp == temp. temp == temp.
			  temp == temp. temp == temp.
			  temp == temp. temp == temp. nil]
		labeled: 'load 1 as a temp, 20 times' repeated: 10000

	"Benchmark new testLoadTempNRef"

"""
    // 9 .. 64
    let expected = [118, 104, 112, 137, 117, 200, 164, 42, 16, 16, 198, 135, 16, 16, 198, 135, 16, 16, 198, 135, 16, 16, 198, 135, 16, 16, 198, 135, 16, 16, 198, 135, 16, 16, 198, 135, 16, 16, 198, 135, 16, 16, 198, 135, 16, 16, 198, 135, 115, 125, 33, 34, 131, 96, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestLoadTempRef() throws {
    let source = """
testLoadTempRef
	| temp |
	temp _ 0@0.
	self test: [temp == temp. temp == temp.
			  temp == temp. temp == temp.
			  temp == temp. temp == temp.
			  temp == temp. temp == temp.
			  temp == temp. temp == temp. nil]
		labeled: 'load 0@0, 20 times' repeated: 10000

	"Benchmark new testLoadTempRef"

"""
    // 9 .. 66
    let expected = [117, 117, 187, 104, 112, 137, 117, 200, 164, 42, 16, 16, 198, 135, 16, 16, 198, 135, 16, 16, 198, 135, 16, 16, 198, 135, 16, 16, 198, 135, 16, 16, 198, 135, 16, 16, 198, 135, 16, 16, 198, 135, 16, 16, 198, 135, 16, 16, 198, 135, 115, 125, 33, 34, 131, 96, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestLoadQuickConstant() throws {
    let source = """
testLoadQuickConstant
	self test: [1 == 1. 1 == 1. 1 == 1. 1 == 1. 1 == 1.
			  1 == 1. 1 == 1. 1 == 1. 1 == 1. 1 == 1.
			  1 == 1. 1 == 1. 1 == 1. 1 == 1. 1 == 1.
			  1 == 1. 1 == 1. 1 == 1. 1 == 1. 1 == 1. nil]
		labeled: 'load 1, 40 times; send ==, 20 times' repeated: 10000

	"Benchmark new testLoadQuickConstant"

"""
    // 9 .. 102
    let expected = [112, 137, 117, 200, 164, 82, 118, 118, 198, 135, 118, 118, 198, 135, 118, 118, 198, 135, 118, 118, 198, 135, 118, 118, 198, 135, 118, 118, 198, 135, 118, 118, 198, 135, 118, 118, 198, 135, 118, 118, 198, 135, 118, 118, 198, 135, 118, 118, 198, 135, 118, 118, 198, 135, 118, 118, 198, 135, 118, 118, 198, 135, 118, 118, 198, 135, 118, 118, 198, 135, 118, 118, 198, 135, 118, 118, 198, 135, 118, 118, 198, 135, 118, 118, 198, 135, 115, 125, 33, 34, 131, 96, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestLoadLiteralNRef() throws {
    let source = """
testLoadLiteralNRef
	self test: [3 == 3. 3 == 3. 3 == 3. 3 == 3. 3 == 3.
			  3 == 3. 3 == 3. 3 == 3. 3 == 3. 3 == 3. nil]
		labeled: 'load nonRefcounted literal, 20 times' repeated: 10000

	"Benchmark new testLoadLiteralNRef"

"""
    // 11 .. 64
    let expected = [112, 137, 117, 200, 164, 42, 33, 33, 198, 135, 33, 33, 198, 135, 33, 33, 198, 135, 33, 33, 198, 135, 33, 33, 198, 135, 33, 33, 198, 135, 33, 33, 198, 135, 33, 33, 198, 135, 33, 33, 198, 135, 33, 33, 198, 135, 115, 125, 34, 35, 131, 96, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestLoadLiteralIndirect() throws {
    let source = """
testLoadLiteralIndirect
	self test: [Point == Point. Point == Point.
			  Point == Point. Point == Point.
			  Point == Point. Point == Point.
			  Point == Point. Point == Point.
			  Point == Point. Point == Point. nil]
		labeled: 'load literal indirect (overflow refct), 20 times' repeated: 10000

	"Benchmark new testLoadLiteralIndirect"

"""
    // 11 .. 64
    let expected = [112, 137, 117, 200, 164, 42, 65, 65, 198, 135, 65, 65, 198, 135, 65, 65, 198, 135, 65, 65, 198, 135, 65, 65, 198, 135, 65, 65, 198, 135, 65, 65, 198, 135, 65, 65, 198, 135, 65, 65, 198, 135, 65, 65, 198, 135, 115, 125, 34, 35, 131, 96, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestPopStoreInstVar() throws {
    let source = """
testPopStoreInstVar
	self test: [dummy _ 1. dummy _ 1. dummy _ 1. dummy _ 1. dummy _ 1.
			  dummy _ 1. dummy _ 1. dummy _ 1. dummy _ 1. dummy _ 1.
			  dummy _ 1. dummy _ 1. dummy _ 1. dummy _ 1. dummy _ 1.
			  dummy _ 1. dummy _ 1. dummy _ 1. dummy _ 1. dummy _ 1. nil]
		labeled: 'store into an instance variable, 20 times' repeated: 10000

	"Benchmark new testPopStoreInstVar"

"""
    // 9 .. 62
    let expected = [112, 137, 117, 200, 164, 42, 118, 96, 118, 96, 118, 96, 118, 96, 118, 96, 118, 96, 118, 96, 118, 96, 118, 96, 118, 96, 118, 96, 118, 96, 118, 96, 118, 96, 118, 96, 118, 96, 118, 96, 118, 96, 118, 96, 118, 96, 115, 125, 33, 34, 131, 96, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestPopStoreTemp() throws {
    let source = """
testPopStoreTemp
	| temp |
	self test: [temp _ 1. temp _ 1. temp _ 1. temp _ 1. temp _ 1.
			  temp _ 1. temp _ 1. temp _ 1. temp _ 1. temp _ 1.
			  temp _ 1. temp _ 1. temp _ 1. temp _ 1. temp _ 1.
			  temp _ 1. temp _ 1. temp _ 1. temp _ 1. temp _ 1. nil]
		labeled: 'store into a temp, 20 times' repeated: 10000

	"Benchmark new testPopStoreTemp"

"""
    // 9 .. 62
    let expected = [112, 137, 117, 200, 164, 42, 118, 104, 118, 104, 118, 104, 118, 104, 118, 104, 118, 104, 118, 104, 118, 104, 118, 104, 118, 104, 118, 104, 118, 104, 118, 104, 118, 104, 118, 104, 118, 104, 118, 104, 118, 104, 118, 104, 118, 104, 115, 125, 33, 34, 131, 96, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTest3plus4() throws {
    let source = """
test3plus4
	self test: [3+4. 3+4. 3+4. 3+4. 3+4. 3+4. 3+4. 3+4. 3+4. 3+4. nil]
		labeled: 'add 3 + 4, 10 times' repeated: 10000

	"Benchmark new test3plus4"

"""
    // 13 .. 66
    let expected = [112, 137, 117, 200, 164, 42, 33, 34, 176, 135, 33, 34, 176, 135, 33, 34, 176, 135, 33, 34, 176, 135, 33, 34, 176, 135, 33, 34, 176, 135, 33, 34, 176, 135, 33, 34, 176, 135, 33, 34, 176, 135, 33, 34, 176, 135, 115, 125, 35, 36, 131, 96, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTest3lessThan4() throws {
    let source = """
test3lessThan4
	self test: [3<4. 3<4. 3<4. 3<4. 3<4. 3<4. 3<4. 3<4. 3<4. 3<4. nil]
		labeled: 'test 3 < 4, 10 times' repeated: 10000

	"Benchmark new test3lessThan4"

"""
    // 13 .. 66
    let expected = [112, 137, 117, 200, 164, 42, 33, 34, 178, 135, 33, 34, 178, 135, 33, 34, 178, 135, 33, 34, 178, 135, 33, 34, 178, 135, 33, 34, 178, 135, 33, 34, 178, 135, 33, 34, 178, 135, 33, 34, 178, 135, 33, 34, 178, 135, 115, 125, 35, 36, 131, 96, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTest3times4() throws {
    let source = """
test3times4
	self test: [3*4. 3*4. 3*4. 3*4. 3*4. 3*4. 3*4. 3*4. 3*4. 3*4. nil]
		labeled: 'multiply 3 * 4, 10 times' repeated: 10000

	"Benchmark new test3times4"

"""
    // 13 .. 66
    let expected = [112, 137, 117, 200, 164, 42, 33, 34, 184, 135, 33, 34, 184, 135, 33, 34, 184, 135, 33, 34, 184, 135, 33, 34, 184, 135, 33, 34, 184, 135, 33, 34, 184, 135, 33, 34, 184, 135, 33, 34, 184, 135, 33, 34, 184, 135, 115, 125, 35, 36, 131, 96, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTest3div4() throws {
    let source = """
test3div4
	self test: [3//4. 3//4. 3//4. 3//4. 3//4. 3//4. 3//4. 3//4. 3//4. 3//4. nil]
		labeled: 'divide 3 by 4, 10 times' repeated: 1000

	"Benchmark new test3div4"

"""
    // 13 .. 66
    let expected = [112, 137, 117, 200, 164, 42, 33, 34, 189, 135, 33, 34, 189, 135, 33, 34, 189, 135, 33, 34, 189, 135, 33, 34, 189, 135, 33, 34, 189, 135, 33, 34, 189, 135, 33, 34, 189, 135, 33, 34, 189, 135, 33, 34, 189, 135, 115, 125, 35, 36, 131, 96, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTest16bitArith() throws {
    let source = """
test16bitArith
	| twentyK |
	twentyK _ 20000.
	self test: [twentyK+twentyK. twentyK+twentyK.
			  twentyK+twentyK. twentyK+twentyK.
			  twentyK+twentyK. twentyK+twentyK.
			  twentyK+twentyK. twentyK+twentyK.
			  twentyK+twentyK. twentyK+twentyK. nil]
		labeled: 'add 20000 + 20000, 10 times' repeated: 1000

	"Benchmark new test16bitArith"

"""
    // 11 .. 66
    let expected = [32, 104, 112, 137, 117, 200, 164, 42, 16, 16, 176, 135, 16, 16, 176, 135, 16, 16, 176, 135, 16, 16, 176, 135, 16, 16, 176, 135, 16, 16, 176, 135, 16, 16, 176, 135, 16, 16, 176, 135, 16, 16, 176, 135, 16, 16, 176, 135, 115, 125, 34, 35, 131, 97, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestLargeIntArith() throws {
    let source = """
testLargeIntArith
	| eightyK |
	eightyK _ 80000.
	self test: [eightyK+eightyK. eightyK+eightyK.
			  eightyK+eightyK. eightyK+eightyK.
			  eightyK+eightyK. eightyK+eightyK.
			  eightyK+eightyK. eightyK+eightyK.
			  eightyK+eightyK. eightyK+eightyK. nil]
		labeled: 'add 80000 + 80000, 10 times' repeated: 100

	"Benchmark new testLargeIntArith"

"""
    // 11 .. 66
    let expected = [32, 104, 112, 137, 117, 200, 164, 42, 16, 16, 176, 135, 16, 16, 176, 135, 16, 16, 176, 135, 16, 16, 176, 135, 16, 16, 176, 135, 16, 16, 176, 135, 16, 16, 176, 135, 16, 16, 176, 135, 16, 16, 176, 135, 16, 16, 176, 135, 115, 125, 34, 35, 131, 97, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestActivationReturn() throws {
    let source = """
testActivationReturn
	self test:
			[self recur: 14]
		labeled: 'activate and return, 32K times' repeated: 1

	"Benchmark new testActivationReturn"

"""
    // 11 .. 26
    let expected = [112, 137, 117, 200, 164, 4, 112, 34, 225, 125, 35, 118, 131, 96, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestShortBranch() throws {
    let source = """
testShortBranch
	self test: [false ifTrue: [1] ifFalse: [2].
			  false ifTrue: [1] ifFalse: [2].
			  false ifTrue: [1] ifFalse: [2].
			  false ifTrue: [1] ifFalse: [2].
			  false ifTrue: [1] ifFalse: [2].
			  false ifTrue: [1] ifFalse: [2].
			  false ifTrue: [1] ifFalse: [2].
			  false ifTrue: [1] ifFalse: [2].
			  false ifTrue: [1] ifFalse: [2].
			  false ifTrue: [1] ifFalse: [2]. nil]
		labeled: 'short branch on false, 10 times' repeated: 10000

	"Benchmark new testShortBranch"

"""
    // 9 .. 82
    let expected = [112, 137, 117, 200, 164, 62, 114, 153, 118, 144, 119, 135, 114, 153, 118, 144, 119, 135, 114, 153, 118, 144, 119, 135, 114, 153, 118, 144, 119, 135, 114, 153, 118, 144, 119, 135, 114, 153, 118, 144, 119, 135, 114, 153, 118, 144, 119, 135, 114, 153, 118, 144, 119, 135, 114, 153, 118, 144, 119, 135, 114, 153, 118, 144, 119, 135, 115, 125, 33, 34, 131, 96, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestWhileLoop() throws {
    let source = """
testWhileLoop
	| temp |
	self test: [temp _ 10000.
			  [temp > 0] whileTrue: [temp _ temp - 1].
			  nil]
		labeled: 'simple whileLoop, 10000 times through' repeated: 10

	"Benchmark new testWhileLoop"

"""
    // 11 .. 36
    let expected = [112, 137, 117, 200, 164, 14, 33, 104, 16, 117, 179, 157, 16, 118, 177, 104, 163, 246, 115, 125, 34, 35, 131, 96, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestArrayAt() throws {
    let source = """
testArrayAt
	| array index |
	array _ #(1 2 3 4 5 6).
	index _ 4.
	self test:
			[array at: index. array at: index. array at: index. array at: index.
			 array at: index. array at: index. array at: index. array at: index.
			 array at: index. array at: index. array at: index. array at: index.
			 array at: index. array at: index. array at: index. array at: index.
			 array at: index. array at: index. array at: index. array at: index. nil]
		labeled: 'send #at: 20 times (to an array)' repeated: 1000

	"Benchmark new testArrayAt"

"""
    // 13 .. 110
    let expected = [32, 104, 33, 105, 112, 137, 117, 200, 164, 82, 16, 17, 192, 135, 16, 17, 192, 135, 16, 17, 192, 135, 16, 17, 192, 135, 16, 17, 192, 135, 16, 17, 192, 135, 16, 17, 192, 135, 16, 17, 192, 135, 16, 17, 192, 135, 16, 17, 192, 135, 16, 17, 192, 135, 16, 17, 192, 135, 16, 17, 192, 135, 16, 17, 192, 135, 16, 17, 192, 135, 16, 17, 192, 135, 16, 17, 192, 135, 16, 17, 192, 135, 16, 17, 192, 135, 16, 17, 192, 135, 115, 125, 35, 36, 131, 98, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestArrayAtPut() throws {
    let source = """
testArrayAtPut
	| array index element |
	array _ #(1 2 3 4 5 6).
	index _ 4.
	element _ 17.
	self test:
			[array at: index put: element. array at: index put: element.
			 array at: index put: element. array at: index put: element.
			 array at: index put: element. array at: index put: element.
			 array at: index put: element. array at: index put: element.
			 array at: index put: element. array at: index put: element.
			 array at: index put: element. array at: index put: element.
			 array at: index put: element. array at: index put: element.
			 array at: index put: element. array at: index put: element.
			 array at: index put: element. array at: index put: element.
			 array at: index put: element. array at: index put: element. nil]
		labeled: 'send #at:put: 20 times (to an array)' repeated: 1000

	"Benchmark new testArrayAtPut"

"""
    // 15 .. 134
    let expected = [32, 104, 33, 105, 34, 106, 112, 137, 117, 200, 164, 102, 16, 17, 18, 193, 135, 16, 17, 18, 193, 135, 16, 17, 18, 193, 135, 16, 17, 18, 193, 135, 16, 17, 18, 193, 135, 16, 17, 18, 193, 135, 16, 17, 18, 193, 135, 16, 17, 18, 193, 135, 16, 17, 18, 193, 135, 16, 17, 18, 193, 135, 16, 17, 18, 193, 135, 16, 17, 18, 193, 135, 16, 17, 18, 193, 135, 16, 17, 18, 193, 135, 16, 17, 18, 193, 135, 16, 17, 18, 193, 135, 16, 17, 18, 193, 135, 16, 17, 18, 193, 135, 16, 17, 18, 193, 135, 16, 17, 18, 193, 135, 115, 125, 36, 37, 131, 99, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestStringAt() throws {
    let source = """
testStringAt
	| string index |
	string _ 'abcdefg'.
	index _ 4.
	self test:
			[string at: index. string at: index. string at: index. string at: index.
			 string at: index. string at: index. string at: index. string at: index.
			 string at: index. string at: index. string at: index. string at: index.
			 string at: index. string at: index. string at: index. string at: index.
			 string at: index. string at: index. string at: index. string at: index. nil]
		labeled: 'send #at: 20 times (to a string)' repeated: 1000

	"Benchmark new testStringAt"

"""
    // 13 .. 110
    let expected = [32, 104, 33, 105, 112, 137, 117, 200, 164, 82, 16, 17, 192, 135, 16, 17, 192, 135, 16, 17, 192, 135, 16, 17, 192, 135, 16, 17, 192, 135, 16, 17, 192, 135, 16, 17, 192, 135, 16, 17, 192, 135, 16, 17, 192, 135, 16, 17, 192, 135, 16, 17, 192, 135, 16, 17, 192, 135, 16, 17, 192, 135, 16, 17, 192, 135, 16, 17, 192, 135, 16, 17, 192, 135, 16, 17, 192, 135, 16, 17, 192, 135, 16, 17, 192, 135, 16, 17, 192, 135, 115, 125, 35, 36, 131, 98, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestStringAtPut() throws {
    let source = """
testStringAtPut
	| string index char |
	string _ 'abcdefg'.
	index _ 4.
	char _ $q.
	self test:
			[string at: index put: char. string at: index put: char.
			 string at: index put: char. string at: index put: char.
			 string at: index put: char. string at: index put: char.
			 string at: index put: char. string at: index put: char.
			 string at: index put: char. string at: index put: char.
			 string at: index put: char. string at: index put: char.
			 string at: index put: char. string at: index put: char.
			 string at: index put: char. string at: index put: char.
			 string at: index put: char. string at: index put: char.
			 string at: index put: char. string at: index put: char. nil]
		labeled: 'send #at:put: 20 times (to a string)' repeated: 1000

	"Benchmark new testStringAtPut"

"""
    // 15 .. 134
    let expected = [32, 104, 33, 105, 34, 106, 112, 137, 117, 200, 164, 102, 16, 17, 18, 193, 135, 16, 17, 18, 193, 135, 16, 17, 18, 193, 135, 16, 17, 18, 193, 135, 16, 17, 18, 193, 135, 16, 17, 18, 193, 135, 16, 17, 18, 193, 135, 16, 17, 18, 193, 135, 16, 17, 18, 193, 135, 16, 17, 18, 193, 135, 16, 17, 18, 193, 135, 16, 17, 18, 193, 135, 16, 17, 18, 193, 135, 16, 17, 18, 193, 135, 16, 17, 18, 193, 135, 16, 17, 18, 193, 135, 16, 17, 18, 193, 135, 16, 17, 18, 193, 135, 16, 17, 18, 193, 135, 16, 17, 18, 193, 135, 115, 125, 36, 37, 131, 99, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestSize() throws {
    let source = """
testSize
	| string |
	string _ 'abcde'.
	self test:
			[string size. string size. string size. string size. string size.
			 string size. string size. string size. string size. string size.
			 string size. string size. string size. string size. string size.
			 string size. string size. string size. string size. string size. nil]
		labeled: 'send #size 20 times (to a string)' repeated: 1000

	"Benchmark new testSize"

"""
    // 11 .. 86
    let expected = [32, 104, 112, 137, 117, 200, 164, 62, 16, 194, 135, 16, 194, 135, 16, 194, 135, 16, 194, 135, 16, 194, 135, 16, 194, 135, 16, 194, 135, 16, 194, 135, 16, 194, 135, 16, 194, 135, 16, 194, 135, 16, 194, 135, 16, 194, 135, 16, 194, 135, 16, 194, 135, 16, 194, 135, 16, 194, 135, 16, 194, 135, 16, 194, 135, 16, 194, 135, 115, 125, 34, 35, 131, 97, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestPointCreation() throws {
    let source = """
testPointCreation
	self test: [3@4. 3@4. 3@4. 3@4. 3@4. 3@4. 3@4. 3@4. 3@4. 3@4. nil]
		labeled: 'create 3@4, 10 times' repeated: 1000

	"Benchmark new testPointCreation"

"""
    // 13 .. 66
    let expected = [112, 137, 117, 200, 164, 42, 33, 34, 187, 135, 33, 34, 187, 135, 33, 34, 187, 135, 33, 34, 187, 135, 33, 34, 187, 135, 33, 34, 187, 135, 33, 34, 187, 135, 33, 34, 187, 135, 33, 34, 187, 135, 33, 34, 187, 135, 115, 125, 35, 36, 131, 96, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestStreamNext() throws {
    let source = """
testStreamNext
	| strm |
	strm _ ReadStream on: 'abcdefghijklmnopqrstuvwxyz'.
	self test: [strm position: 0.
			  strm next. strm next. strm next. strm next.
			  strm next. strm next. strm next. strm next.
			  strm next. strm next. strm next. strm next.
			  strm next. strm next. strm next. strm next.
			  strm next. strm next. strm next. strm next. nil]
		labeled: 'execute ReadStream next, 20 times' repeated: 1000

	"Benchmark new testStreamNext"

"""
    // 17 .. 98
    let expected = [65, 34, 224, 104, 112, 137, 117, 200, 164, 66, 16, 117, 228, 135, 16, 195, 135, 16, 195, 135, 16, 195, 135, 16, 195, 135, 16, 195, 135, 16, 195, 135, 16, 195, 135, 16, 195, 135, 16, 195, 135, 16, 195, 135, 16, 195, 135, 16, 195, 135, 16, 195, 135, 16, 195, 135, 16, 195, 135, 16, 195, 135, 16, 195, 135, 16, 195, 135, 16, 195, 135, 16, 195, 135, 115, 125, 37, 38, 131, 99, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestStreamNextPut() throws {
    let source = """
testStreamNextPut
	| strm ch |
	strm _ ReadWriteStream on: 'abcdefghijklmnopqrstuvwxyz'.
	ch _ $q.
	self test: [strm position: 0.
			  strm nextPut: ch. strm nextPut: ch. strm nextPut: ch. strm nextPut: ch.
			  strm nextPut: ch. strm nextPut: ch. strm nextPut: ch. strm nextPut: ch.
			  strm nextPut: ch. strm nextPut: ch. strm nextPut: ch. strm nextPut: ch.
			  strm nextPut: ch. strm nextPut: ch. strm nextPut: ch. strm nextPut: ch.
			  strm nextPut: ch. strm nextPut: ch. strm nextPut: ch. strm nextPut: ch. nil]
		labeled: 'execute ReadWriteStream nextPut:, 20 times' repeated: 1000

	"Benchmark new testStreamNextPut"

"""
    // 19 .. 122
    let expected = [65, 34, 224, 104, 35, 105, 112, 137, 117, 200, 164, 86, 16, 117, 229, 135, 16, 17, 196, 135, 16, 17, 196, 135, 16, 17, 196, 135, 16, 17, 196, 135, 16, 17, 196, 135, 16, 17, 196, 135, 16, 17, 196, 135, 16, 17, 196, 135, 16, 17, 196, 135, 16, 17, 196, 135, 16, 17, 196, 135, 16, 17, 196, 135, 16, 17, 196, 135, 16, 17, 196, 135, 16, 17, 196, 135, 16, 17, 196, 135, 16, 17, 196, 135, 16, 17, 196, 135, 16, 17, 196, 135, 16, 17, 196, 135, 115, 125, 38, 39, 131, 100, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestEQ() throws {
    let source = """
testEQ
	| temp |
	temp _ 1.
	self test: [temp == temp == temp == temp == temp ==
			  temp == temp == temp == temp == temp ==
			  temp == temp == temp == temp == temp ==
			  temp == temp == temp == temp == temp. nil]
		labeled: 'send ==, 20 times' repeated: 10000

	"Benchmark new testEQ"

"""
    // 9 .. 64
    let expected = [118, 104, 112, 137, 117, 200, 164, 42, 16, 16, 198, 16, 198, 16, 198, 16, 198, 16, 198, 16, 198, 16, 198, 16, 198, 16, 198, 16, 198, 16, 198, 16, 198, 16, 198, 16, 198, 16, 198, 16, 198, 16, 198, 16, 198, 16, 198, 135, 115, 125, 33, 34, 131, 96, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestClass() throws {
    let source = """
testClass
	| pt |
	pt _ 0@0.
	self test:
			[pt class. pt class. pt class. pt class. pt class.
			 pt class. pt class. pt class. pt class. pt class.
			 pt class. pt class. pt class. pt class. pt class.
			 pt class. pt class. pt class. pt class. pt class. nil]
		labeled: 'send #class 20 times (to a point)' repeated: 1000

	"Benchmark new testClass"

"""
    // 9 .. 86
    let expected = [117, 117, 187, 104, 112, 137, 117, 200, 164, 62, 16, 199, 135, 16, 199, 135, 16, 199, 135, 16, 199, 135, 16, 199, 135, 16, 199, 135, 16, 199, 135, 16, 199, 135, 16, 199, 135, 16, 199, 135, 16, 199, 135, 16, 199, 135, 16, 199, 135, 16, 199, 135, 16, 199, 135, 16, 199, 135, 16, 199, 135, 16, 199, 135, 16, 199, 135, 16, 199, 135, 115, 125, 33, 34, 131, 96, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestBlockCopy() throws {
    let source = """
testBlockCopy
	| tC |
	tC _ thisContext.
	self test: [tC blockCopy: 0. tC blockCopy: 0. tC blockCopy: 0. tC blockCopy: 0.
			  tC blockCopy: 0. tC blockCopy: 0. tC blockCopy: 0. tC blockCopy: 0.
			  tC blockCopy: 0. tC blockCopy: 0. tC blockCopy: 0. tC blockCopy: 0.
			  tC blockCopy: 0. tC blockCopy: 0. tC blockCopy: 0. tC blockCopy: 0.
			  tC blockCopy: 0. tC blockCopy: 0. tC blockCopy: 0. tC blockCopy: 0. nil]
		labeled: 'execute blockCopy: 0, 20 times' repeated: 1000

	"Benchmark new testBlockCopy"

"""
    // 9 .. 104
    let expected = [137, 104, 112, 137, 117, 200, 164, 82, 16, 117, 200, 135, 16, 117, 200, 135, 16, 117, 200, 135, 16, 117, 200, 135, 16, 117, 200, 135, 16, 117, 200, 135, 16, 117, 200, 135, 16, 117, 200, 135, 16, 117, 200, 135, 16, 117, 200, 135, 16, 117, 200, 135, 16, 117, 200, 135, 16, 117, 200, 135, 16, 117, 200, 135, 16, 117, 200, 135, 16, 117, 200, 135, 16, 117, 200, 135, 16, 117, 200, 135, 16, 117, 200, 135, 16, 117, 200, 135, 115, 125, 33, 34, 131, 96, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestValue() throws {
    let source = """
testValue
	| block |
	block _ [3+4].
	self test: [block value. block value. block value. block value.
			  block value. block value. block value. block value.
			  block value. block value. block value. block value.
			  block value. block value. block value. block value.
			  block value. block value. block value. block value. nil]
		labeled: 'evaluate the block: (3+4), 20 times' repeated: 1000

	"Benchmark new testValue"

"""
    // 13 .. 96
    let expected = [137, 117, 200, 164, 4, 32, 33, 176, 125, 104, 112, 137, 117, 200, 164, 62, 16, 201, 135, 16, 201, 135, 16, 201, 135, 16, 201, 135, 16, 201, 135, 16, 201, 135, 16, 201, 135, 16, 201, 135, 16, 201, 135, 16, 201, 135, 16, 201, 135, 16, 201, 135, 16, 201, 135, 16, 201, 135, 16, 201, 135, 16, 201, 135, 16, 201, 135, 16, 201, 135, 16, 201, 135, 16, 201, 135, 115, 125, 35, 36, 131, 98, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestCreation() throws {
    let source = """
testCreation
	self test:
			[Point new. Point new. Point new. Point new. Point new.
			  Point new. Point new. Point new. Point new. Point new.
			  Point new. Point new. Point new. Point new. Point new.
			  Point new. Point new. Point new. Point new. Point new. nil]
		labeled: 'create 20 uninitialized points' repeated: 1000

	"Benchmark new testCreation"

"""
    // 11 .. 84
    let expected = [112, 137, 117, 200, 164, 62, 65, 204, 135, 65, 204, 135, 65, 204, 135, 65, 204, 135, 65, 204, 135, 65, 204, 135, 65, 204, 135, 65, 204, 135, 65, 204, 135, 65, 204, 135, 65, 204, 135, 65, 204, 135, 65, 204, 135, 65, 204, 135, 65, 204, 135, 65, 204, 135, 65, 204, 135, 65, 204, 135, 65, 204, 135, 65, 204, 135, 115, 125, 34, 35, 131, 96, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestPointX() throws {
    let source = """
testPointX
	| pt |
	pt _ 0@0.
	self test: [pt x. pt x. pt x. pt x. pt x. pt x. pt x. pt x. pt x. pt x. nil]
		labeled: 'execute aPoint x, 10 times' repeated: 10000

	"Benchmark new testPointX"

"""
    // 9 .. 56
    let expected = [117, 117, 187, 104, 112, 137, 117, 200, 164, 32, 16, 206, 135, 16, 206, 135, 16, 206, 135, 16, 206, 135, 16, 206, 135, 16, 206, 135, 16, 206, 135, 16, 206, 135, 16, 206, 135, 16, 206, 135, 115, 125, 33, 34, 131, 96, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestLoadThisContext() throws {
    let source = """
testLoadThisContext
	self test: [thisContext == thisContext. thisContext == thisContext.
			  thisContext == thisContext. thisContext == thisContext.
			  thisContext == thisContext. thisContext == thisContext.
			  thisContext == thisContext. thisContext == thisContext.
			  thisContext == thisContext. thisContext == thisContext. nil]
		labeled: 'load thisContext, 20 times' repeated: 10000

	"Benchmark new testLoadThisContext"

"""
    // 9 .. 62
    let expected = [112, 137, 117, 200, 164, 42, 137, 137, 198, 135, 137, 137, 198, 135, 137, 137, 198, 135, 137, 137, 198, 135, 137, 137, 198, 135, 137, 137, 198, 135, 137, 137, 198, 135, 137, 137, 198, 135, 137, 137, 198, 135, 137, 137, 198, 135, 115, 125, 33, 34, 131, 96, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestBasicAt() throws {
    let source = """
testBasicAt
	| coll index |
	coll _ Set new: 16.
	index _ 5.
	self test:
			[coll basicAt: index. coll basicAt: index. coll basicAt: index. coll basicAt: index.
			 coll basicAt: index. coll basicAt: index. coll basicAt: index. coll basicAt: index.
			 coll basicAt: index. coll basicAt: index. coll basicAt: index. coll basicAt: index.
			 coll basicAt: index. coll basicAt: index. coll basicAt: index. coll basicAt: index.
			 coll basicAt: index. coll basicAt: index. coll basicAt: index. coll basicAt: index. nil]
		labeled: 'send #basicAt: 20 times (to a set)' repeated: 1000

	"Benchmark new testBasicAt"

"""
    // 17 .. 116
    let expected = [64, 33, 205, 104, 34, 105, 112, 137, 117, 200, 164, 82, 16, 17, 228, 135, 16, 17, 228, 135, 16, 17, 228, 135, 16, 17, 228, 135, 16, 17, 228, 135, 16, 17, 228, 135, 16, 17, 228, 135, 16, 17, 228, 135, 16, 17, 228, 135, 16, 17, 228, 135, 16, 17, 228, 135, 16, 17, 228, 135, 16, 17, 228, 135, 16, 17, 228, 135, 16, 17, 228, 135, 16, 17, 228, 135, 16, 17, 228, 135, 16, 17, 228, 135, 16, 17, 228, 135, 16, 17, 228, 135, 115, 125, 37, 38, 131, 99, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestBasicAtPut() throws {
    let source = """
testBasicAtPut
	| coll index element |
	coll _ Set new: 16.
	index _ 5.
	element _ 17.
	self test:
			[coll basicAt: index put: element. coll basicAt: index put: element.
			 coll basicAt: index put: element. coll basicAt: index put: element.
			 coll basicAt: index put: element. coll basicAt: index put: element.
			 coll basicAt: index put: element. coll basicAt: index put: element.
			 coll basicAt: index put: element. coll basicAt: index put: element.
			 coll basicAt: index put: element. coll basicAt: index put: element.
			 coll basicAt: index put: element. coll basicAt: index put: element.
			 coll basicAt: index put: element. coll basicAt: index put: element.
			 coll basicAt: index put: element. coll basicAt: index put: element.
			 coll basicAt: index put: element. coll basicAt: index put: element. nil]
		labeled: 'send #basicAtPut: 20 times (to a set)' repeated: 1000

	"Benchmark new testBasicAtPut"

"""
    // 19 .. 140
    let expected = [64, 33, 205, 104, 34, 105, 35, 106, 112, 137, 117, 200, 164, 102, 16, 17, 18, 245, 135, 16, 17, 18, 245, 135, 16, 17, 18, 245, 135, 16, 17, 18, 245, 135, 16, 17, 18, 245, 135, 16, 17, 18, 245, 135, 16, 17, 18, 245, 135, 16, 17, 18, 245, 135, 16, 17, 18, 245, 135, 16, 17, 18, 245, 135, 16, 17, 18, 245, 135, 16, 17, 18, 245, 135, 16, 17, 18, 245, 135, 16, 17, 18, 245, 135, 16, 17, 18, 245, 135, 16, 17, 18, 245, 135, 16, 17, 18, 245, 135, 16, 17, 18, 245, 135, 16, 17, 18, 245, 135, 16, 17, 18, 245, 135, 115, 125, 38, 39, 131, 100, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestPerform() throws {
    let source = """
testPerform
	self test: [3 perform: #+ with: 4. 3 perform: #+ with: 4.
			  3 perform: #+ with: 4. 3 perform: #+ with: 4.
			  3 perform: #+ with: 4. 3 perform: #+ with: 4.
			  3 perform: #+ with: 4. 3 perform: #+ with: 4.
			  3 perform: #+ with: 4. 3 perform: #+ with: 4.
			  3 perform: #+ with: 4. 3 perform: #+ with: 4.
			  3 perform: #+ with: 4. 3 perform: #+ with: 4.
			  3 perform: #+ with: 4. 3 perform: #+ with: 4.
			  3 perform: #+ with: 4. 3 perform: #+ with: 4.
			  3 perform: #+ with: 4. 3 perform: #+ with: 4. nil]
		labeled: '3 perform: #+ with: 4, 20 times' repeated: 1000

	"Benchmark new testPerform"

"""
    // 17 .. 130
    let expected = [112, 137, 117, 200, 164, 102, 34, 35, 36, 241, 135, 34, 35, 36, 241, 135, 34, 35, 36, 241, 135, 34, 35, 36, 241, 135, 34, 35, 36, 241, 135, 34, 35, 36, 241, 135, 34, 35, 36, 241, 135, 34, 35, 36, 241, 135, 34, 35, 36, 241, 135, 34, 35, 36, 241, 135, 34, 35, 36, 241, 135, 34, 35, 36, 241, 135, 34, 35, 36, 241, 135, 34, 35, 36, 241, 135, 34, 35, 36, 241, 135, 34, 35, 36, 241, 135, 34, 35, 36, 241, 135, 34, 35, 36, 241, 135, 34, 35, 36, 241, 135, 34, 35, 36, 241, 135, 115, 125, 37, 38, 131, 96, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestStringReplace() throws {
    let source = """
testStringReplace
	| source sink size |
	source _ self longishString copy.
	sink _ source copy.
	size _ sink size.
	self test:
			[sink replaceFrom: 1 to: size with: source startingAt: 1. nil]
		labeled: 'replace characters in a string' repeated: 100

	"Benchmark new testStringReplace"

"""
    // 15 .. 46
    let expected = [112, 209, 208, 104, 16, 208, 105, 17, 194, 106, 112, 137, 117, 200, 164, 10, 17, 118, 18, 16, 118, 131, 131, 135, 115, 125, 36, 37, 131, 98, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestAsFloat() throws {
    let source = """
testAsFloat
	self test: [1 asFloat. 1 asFloat. 1 asFloat. 1 asFloat. 1 asFloat.
			  1 asFloat. 1 asFloat. 1 asFloat. 1 asFloat. 1 asFloat.
			  1 asFloat. 1 asFloat. 1 asFloat. 1 asFloat. 1 asFloat.
			  1 asFloat. 1 asFloat. 1 asFloat. 1 asFloat. 1 asFloat. nil]
		labeled: 'convert 1 to floating point, 20 times' repeated: 100

	"Benchmark new testAsFloat"

"""
    // 11 .. 84
    let expected = [112, 137, 117, 200, 164, 62, 118, 209, 135, 118, 209, 135, 118, 209, 135, 118, 209, 135, 118, 209, 135, 118, 209, 135, 118, 209, 135, 118, 209, 135, 118, 209, 135, 118, 209, 135, 118, 209, 135, 118, 209, 135, 118, 209, 135, 118, 209, 135, 118, 209, 135, 118, 209, 135, 118, 209, 135, 118, 209, 135, 118, 209, 135, 118, 209, 135, 115, 125, 34, 35, 131, 96, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestFloatingPointAddition() throws {
    let source = """
testFloatingPointAddition
	| a b |
	a _ 3.1.
	b _ 4.1.
	self test: [a+b. a+b. a+b. a+b. a+b. a+b. a+b. a+b. a+b. a+b.
			  a+b. a+b. a+b. a+b. a+b. a+b. a+b. a+b. a+b. a+b. nil]
		labeled: 'add 3.1 plus 4.1, 20 times' repeated: 100

	"Benchmark new testFloatingPointAddition"

"""
    // 13 .. 110
    let expected = [32, 104, 33, 105, 112, 137, 117, 200, 164, 82, 16, 17, 176, 135, 16, 17, 176, 135, 16, 17, 176, 135, 16, 17, 176, 135, 16, 17, 176, 135, 16, 17, 176, 135, 16, 17, 176, 135, 16, 17, 176, 135, 16, 17, 176, 135, 16, 17, 176, 135, 16, 17, 176, 135, 16, 17, 176, 135, 16, 17, 176, 135, 16, 17, 176, 135, 16, 17, 176, 135, 16, 17, 176, 135, 16, 17, 176, 135, 16, 17, 176, 135, 16, 17, 176, 135, 16, 17, 176, 135, 115, 125, 35, 36, 131, 98, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestBitBLT() throws {
    let source = """
testBitBLT
	| bLTer |
	bLTer _ self sampleBitBLT.
	self test:
			[bLTer copyBits. bLTer copyBits.
			 bLTer copyBits. bLTer copyBits.
			 bLTer copyBits. bLTer copyBits.
			 bLTer copyBits. bLTer copyBits.
			 bLTer copyBits. bLTer copyBits. nil]
		labeled: 'call bitBLT 10 times' repeated: 10

	"Benchmark new testBitBLT"

"""
    // 13 .. 59
    let expected = [112, 208, 104, 112, 137, 117, 200, 164, 32, 16, 210, 135, 16, 210, 135, 16, 210, 135, 16, 210, 135, 16, 210, 135, 16, 210, 135, 16, 210, 135, 16, 210, 135, 16, 210, 135, 16, 210, 135, 115, 125, 35, 36, 131, 97, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestTextScanning() throws {
    let source = """
testTextScanning
	| clipRect para range scanner stops |
	clipRect _ Display boundingBox.
	para _ Paragraph withText: 'Hi there, folks' asText.
	range _ 1 to: para numberOfLines.
	scanner _ DisplayScanner new.
	scanner displayLines: range in: para clippedBy: clipRect.
	stops _ scanner instVarAt: 17.
	self test:
		[scanner destX: 0.
		 scanner
			scanCharactersFrom: 1
			to: 16
			in: 'Hi there, folks!'
			rightX: 400
			stopConditions: stops
			displaying: true]
		labeled: 'scan characters (primitive text display)' repeated: 100

	"Benchmark new testTextScanning"

"""
    // 43 .. 95
    let expected = [65, 208, 104, 67, 37, 212, 226, 105, 118, 17, 215, 230, 106, 72, 204, 107, 19, 18, 17, 16, 131, 105, 135, 19, 43, 234, 108, 112, 137, 117, 200, 164, 14, 19, 117, 237, 135, 19, 118, 47, 48, 49, 20, 113, 131, 206, 125, 50, 51, 131, 108, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestClassOrganizer() throws {
    let source = """
testClassOrganizer
	| class |
	class _ Benchmark.
	self test:
			[class organization changeFromString: class organization printString]
		labeled: 'read and write class organization' repeated: 1

	"Benchmark new testClassOrganizer"

"""
    // 15 .. 35
    let expected = [64, 104, 112, 137, 117, 200, 164, 7, 16, 211, 16, 211, 212, 226, 125, 37, 118, 131, 97, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestPrintDefinition() throws {
    let source = """
testPrintDefinition
	| class |
	class _ Compiler.
	self test:
			[class definition]
		labeled: 'print a class definition' repeated: 20

	"Benchmark new testPrintDefinition"

"""
    // 13 .. 29
    let expected = [64, 104, 112, 137, 117, 200, 164, 3, 16, 210, 125, 35, 36, 131, 97, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestPrintHierarchy() throws {
    let source = """
testPrintHierarchy
	| class |
	class _ InstructionStream.
	self test:
			[class printHierarchy]
		labeled: 'print a class hierarchy' repeated: 10

	"Benchmark new testPrintHierarchy"

"""
    // 13 .. 29
    let expected = [64, 104, 112, 137, 117, 200, 164, 3, 16, 210, 125, 35, 36, 131, 97, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestAllCallsOn() throws {
    let source = """
testAllCallsOn
	self test:
			[Smalltalk allCallsOn: #printStringRadix:]
		labeled: 'find all calls on #printStringRadix:' repeated: 1

	"Benchmark new testAllCallsOn"

"""
    // 13 .. 28
    let expected = [112, 137, 117, 200, 164, 4, 66, 35, 225, 125, 36, 118, 131, 96, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestAllImplementors() throws {
    let source = """
testAllImplementors
	self test:
			[Smalltalk allImplementorsOf: #next]
		labeled: 'find all implementors of #next' repeated: 1

	"Benchmark new testAllImplementors"

"""
    // 13 .. 28
    let expected = [112, 137, 117, 200, 164, 4, 66, 35, 225, 125, 36, 118, 131, 96, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestInspect() throws {
    let source = """
testInspect
	| window |
	self test:
			[window _ InspectorView buildScheduledView:
							(Inspector inspect: Compiler new).
			  window release]
		labeled: 'create an inspector view' repeated: 10

	"Benchmark new testInspect"

"""
    // 21 .. 42
    let expected = [112, 137, 117, 200, 164, 10, 66, 68, 69, 204, 227, 225, 104, 16, 214, 125, 39, 40, 131, 96, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestCompiler() throws {
    let source = """
testCompiler
	self test:
			[Benchmark compile:
'dummy: aBlock repeated: nTimes | i emptyBlock emptyTime blockTime |
	nTimes > 1000 ifTrue: [^self time: aBlock repeatedK: nTimes // 1000].
	emptyBlock _ [].
	emptyTime _ Time millisecondsToRun:
					[i _ 0.
					 [(i _ i + 1) <= nTimes] whileTrue: [emptyBlock value]].
	blockTime _ Time millisecondsToRun:
					[i _ 0.
					 [(i _ i + 1) <= nTimes] whileTrue: [aBlock value]].
	^blockTime - emptyTime'
				notifying: nil trailer: #(0 0 0)]
		labeled: 'compile dummy method' repeated: 5.
	Benchmark removeSelector: #dummy:repeated:

	"Benchmark new testCompiler"

"""
    // 21 .. 43
    let expected = [112, 137, 117, 200, 164, 7, 66, 35, 115, 36, 131, 97, 125, 37, 38, 131, 96, 135, 66, 40, 231, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestDecompiler() throws {
    let source = """
testDecompiler
	| class |
	class _ InputSensor.
	self
		test: [class selectors do:
				[:sel | (Decompiler new
						decompile: sel
						in: class
						method: (class compiledMethodAt: sel)) decompileString]]
		labeled: 'decompile class InputSensor' repeated: 1

	"Benchmark new testDecompiler"

"""
    // 19 .. 53
    let expected = [64, 104, 112, 137, 117, 200, 164, 21, 16, 210, 137, 118, 200, 164, 12, 105, 69, 204, 17, 16, 16, 17, 230, 131, 100, 211, 125, 203, 125, 39, 118, 131, 97, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestKeyboardLookAhead() throws {
    let source = """
testKeyboardLookAhead
	| aStringHolderView insert inputter editor |
	aStringHolderView _ self favoriteStringHolderView.
	editor _ aStringHolderView controller.
	editor selectAt: 5.
	inputter _ editor sensor class classPool at: #CurrentInputState.
	Sensor flushKeyboard.
	self test:
			['Now! ' do:
				[:char |
				 inputter keyAt: char asciiValue put: 1].
				 editor readKeyboard]
		labeled: 'text keyboard response using lookahead buffer' repeated: 3.
	aStringHolderView release

	"Benchmark new testKeyboardLookAhead"

"""
    // 37 .. 90
    let expected = [112, 208, 104, 16, 209, 107, 19, 35, 226, 135, 19, 213, 199, 212, 38, 192, 106, 72, 215, 135, 112, 137, 117, 200, 164, 18, 42, 137, 118, 200, 164, 7, 108, 18, 20, 220, 118, 251, 125, 203, 135, 19, 221, 125, 46, 47, 131, 105, 135, 16, 131, 16, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestKeyboardSingle() throws {
    let source = """
testKeyboardSingle
	| aStringHolderView insert inputter editor |
	aStringHolderView _ self favoriteStringHolderView.
	editor _ aStringHolderView controller.
	editor selectAt: 5.
	inputter _ editor sensor class classPool at: #CurrentInputState.
	Sensor flushKeyboard.
	self test:
			['Now! ' do:
				[:char |
				 inputter keyAt: char asciiValue put: 1.
				 editor readKeyboard]]
		labeled: 'text keyboard response for single keystroke' repeated: 3.
	aStringHolderView release

	"Benchmark new testKeyboardSingle"

"""
    // 37 .. 90
    let expected = [112, 208, 104, 16, 209, 107, 19, 35, 226, 135, 19, 213, 199, 212, 38, 192, 106, 72, 215, 135, 112, 137, 117, 200, 164, 18, 42, 137, 118, 200, 164, 10, 108, 18, 20, 220, 118, 251, 135, 19, 221, 125, 203, 125, 46, 47, 131, 105, 135, 16, 131, 16, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestTextDisplay() throws {
    let source = """
testTextDisplay
	| para |
	para _ Paragraph withText: self textForDisplay.
	para displayAt: 200@200.
	self test: [para displayAt: 200@200]
		labeled: 'display text' repeated: 10

	"Benchmark new testTextDisplay"

"""
    // 19 .. 47
    let expected = [65, 112, 210, 224, 104, 16, 36, 36, 187, 227, 135, 112, 137, 117, 200, 164, 6, 16, 36, 36, 187, 227, 125, 38, 39, 131, 101, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestTextFormatting() throws {
    let source = """
testTextFormatting
	| aString aView contents |
	aString _ self stringForDisplay.
	aView _ StringHolderView container: StringHolder new.
	aView window: Display boundingBox viewport: (100@100 extent: 200@200).
	self test:
			[aView editString: (aString asText makeSelectorBoldIn: Benchmark) asParagraph]
		labeled: 'format a bunch of text' repeated: 5.
	aView release

	"Benchmark new testTextFormatting"

"""
    // 41 .. 84
    let expected = [112, 208, 104, 66, 67, 204, 225, 105, 17, 70, 213, 40, 40, 187, 41, 41, 187, 231, 244, 135, 112, 137, 117, 200, 164, 8, 17, 16, 222, 79, 237, 220, 235, 125, 48, 49, 131, 106, 135, 17, 131, 18, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestTextEditing() throws {
    let source = """
testTextEditing
	| aStringHolderView editor selectPoint |
	aStringHolderView _ self favoriteStringHolderView.
	editor _ aStringHolderView controller.
	selectPoint _ 0.
	self test:
			[editor selectAt: (selectPoint _ selectPoint + 5).
			 editor replaceSelectionWith: 'Now! ' asText]
		labeled: 'text replacement and redisplay' repeated: 20.
	aStringHolderView release

	"Benchmark new testTextEditing"

"""
    // 25 .. 60
    let expected = [112, 208, 104, 16, 209, 105, 117, 106, 112, 137, 117, 200, 164, 13, 17, 18, 36, 176, 129, 66, 227, 135, 17, 39, 214, 229, 125, 40, 41, 131, 98, 135, 16, 218, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testTestListToFile() throws {
    let source = """
testList: selectorList toFile: aFileStream
	"This message allows you to test a series of benchmarks and record all the results on
	a file or stream."

	fromList _ true.
	self fileOutputParameters: aFileStream.
	selectorList do:
		[:selector | self perform: selector.
		 verboseTranscript ifTrue:
			[Transcript show: 'press any mouse button to continue'.
			 Sensor waitButton.
			 Transcript clear; refresh]].
	reporting ifTrue: [self closeOutput: reportStream].
	fromList _ false.

	"Benchmark new testList: #(testLoadInstVar testLoadLiteralIndirect testLoadLiteralNRef testLoadQuickConstant testLoadTempNRef) toFile: (FileStream fileNamed: 'Smalltalk.timing')"

"""
    // 23 .. 68
    let expected = [113, 100, 112, 17, 224, 135, 16, 137, 118, 200, 164, 23, 106, 112, 18, 225, 135, 1, 172, 13, 67, 36, 226, 135, 70, 213, 135, 67, 136, 215, 135, 216, 144, 115, 125, 203, 135, 2, 155, 112, 3, 233, 135, 114, 100, 120]
    try runningSource(source, expecting: expected)
  }

  func testDefaultOutputParameters() throws {
    let source = """
defaultOutputParameters
	reporting _ false.
	verboseTranscript _ true.
	fromList _ false

"""
    // 3 .. 9
    let expected = [114, 98, 113, 97, 114, 100, 120]
    try runningSource(source, expecting: expected)
  }

}
