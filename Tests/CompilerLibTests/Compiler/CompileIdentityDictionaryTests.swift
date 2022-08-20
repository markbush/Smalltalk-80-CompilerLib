import XCTest
@testable import CompilerLib

final class CompileIdentityDictionaryTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("IdentityDictionary", instanceVariables: ["tally", "valueArray"])
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

  func testDo() throws {
    let source = """
do: aBlock
	1 to: self basicSize do:
		[:index |
		(self basicAt: index) == nil ifFalse: [aBlock value: (valueArray at: index)]]

"""
    // 9 .. 34
    let expected = [118, 112, 209, 137, 118, 200, 164, 15, 105, 112, 17, 226, 115, 198, 153, 115, 148, 16, 1, 17, 192, 202, 125, 240, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testNoCheckAdd() throws {
    let source = """
noCheckAdd: anAssociation
	^self noCheckAdd: anAssociation key with: anAssociation value

"""
    // 7 .. 13
    let expected = [112, 16, 209, 16, 201, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testFindKeyOrNil() throws {
    let source = """
findKeyOrNil: key
	| index length probe pass |
	length _ self basicSize.
	pass _ 1.
	index _ key asOop \\\\ length + 1.
	[(probe _ self basicAt: index) == nil or: [probe == key]]
		whileFalse: [(index _ index + 1) > length
				ifTrue:
					[index _ 1.
					pass _ pass + 1.
					pass > 2 ifTrue: [^self grow findKeyOrNil: key]]].
	^index

"""
    // 13 .. 67
    let expected = [112, 208, 106, 118, 108, 16, 209, 18, 186, 118, 176, 105, 112, 17, 228, 129, 67, 115, 198, 153, 113, 146, 19, 16, 198, 168, 26, 17, 118, 176, 129, 65, 18, 179, 172, 15, 118, 105, 20, 118, 176, 108, 20, 119, 179, 156, 112, 211, 16, 226, 124, 163, 215, 17, 124]
    try runningSource(source, expecting: expected)
  }

  func testSetTally() throws {
    let source = """
setTally
	super setTally.
	valueArray _ Array new: self basicSize

"""
    // 11 .. 20
    let expected = [112, 133, 0, 135, 65, 112, 210, 205, 97, 120]
    try runningSource(source, expecting: expected)
  }

  func testNoCheckAddWith() throws {
    let source = """
noCheckAdd: key with: value
	| index |
	index _ self findKeyOrNil: key.
	self basicAt: index put: key.
	tally _ tally + 1.
	^valueArray at: index put: value

"""
    // 7 .. 24
    let expected = [112, 16, 224, 106, 112, 18, 16, 241, 135, 0, 118, 176, 96, 1, 18, 17, 193, 124]
    try runningSource(source, expecting: expected)
  }

  func testRemoveDangerouslyKeyIfAbsent() throws {
    let source = """
removeDangerouslyKey: key ifAbsent: errorBlock
	| location oldKey length entry |
	location _ self findKeyOrNil: key.
	(self basicAt: location) == nil ifTrue: [^errorBlock value].
	self basicAt: location put: nil.
	valueArray at: location put: nil.
	tally _ tally - 1.
	length _ self basicSize.
	[location _
		location = length
			ifTrue: [1]
			ifFalse: [location + 1].
	(self basicAt: location) == nil]
		whileFalse:
			[oldKey _ self findKeyOrNil: (self basicAt: location).
			location = oldKey ifFalse: [self swap: location with: oldKey]]

"""
    // 13 .. 78
    let expected = [112, 16, 224, 106, 112, 18, 225, 115, 198, 154, 17, 201, 124, 112, 18, 115, 242, 135, 1, 18, 115, 193, 135, 0, 118, 177, 96, 112, 211, 108, 18, 20, 182, 153, 118, 146, 18, 118, 176, 106, 112, 18, 225, 115, 198, 168, 18, 112, 112, 18, 225, 224, 107, 18, 19, 182, 168, 5, 112, 18, 19, 244, 135, 163, 221, 120]
    try runningSource(source, expecting: expected)
  }

// TODO: adds extra literal (due to super?)
//   func testSwapWith() throws {
//     let source = """
// swap: oneElement with: otherElement
// 	super swap: oneElement with: otherElement.
// 	valueArray swap: oneElement with: otherElement
//
// """
//     // 9 .. 20
//     let expected = [112, 16, 17, 133, 64, 135, 1, 16, 17, 241, 135, 120]
//     try runningSource(source, expecting: expected)
//   }

  func testCopy() throws {
    let source = """
copy
	| v copy |
	v _ valueArray.
	valueArray _ valueArray shallowCopy.
	copy _ super shallowCopy.
	valueArray _ v.
	^copy

"""
    // 7 .. 19
    let expected = [1, 104, 1, 208, 97, 112, 133, 0, 105, 16, 97, 17, 124]
    try runningSource(source, expecting: expected)
  }

  func testAssociationAtIfAbsent() throws {
    let source = """
associationAt: key ifAbsent: aBlock
	"Answer with an Association consisting of key and my value at that key.
	If key is not found, evaluate aBlock."

	| index |
	index _ self findKey: key ifAbsent: [^aBlock value].
	^Association
		key: (self basicAt: index)
		value: (valueArray at: index)

"""
    // 11 .. 31
    let expected = [112, 16, 137, 117, 200, 164, 3, 17, 201, 124, 240, 106, 66, 112, 18, 227, 1, 18, 192, 241, 124]
    try runningSource(source, expecting: expected)
  }

  func testKeyAtValueIfAbsent() throws {
    let source = """
keyAtValue: value ifAbsent: exceptionBlock
	"Answer the key whose value equals the argument, value.  If there is none,
	answer the result of evaluating exceptionBlock."

	| theKey |
	1 to: self basicSize do:
		[:index |
		value == (valueArray at: index)
			ifTrue:
				[(theKey _ self basicAt: index) == nil
					ifFalse: [^theKey]]].
	^exceptionBlock value

"""
    // 9 .. 44
    let expected = [118, 112, 209, 137, 118, 200, 164, 23, 107, 16, 1, 19, 192, 198, 172, 13, 112, 19, 226, 129, 66, 115, 198, 153, 115, 145, 18, 124, 144, 115, 125, 240, 135, 17, 201, 124]
    try runningSource(source, expecting: expected)
  }

  func testAtPut() throws {
    let source = """
at: key put: value
	"Set the value at key to be value."

	| index |
	index _ self findKeyOrNil: key.
	(self basicAt: index) == nil
		ifTrue:
			[tally _ tally + 1.
			self basicAt: index put: key].
	valueArray at: index put: value.
	self fullCheck.
	^value

"""
    // 11 .. 40
    let expected = [112, 16, 224, 106, 112, 18, 226, 115, 198, 172, 9, 0, 118, 176, 96, 112, 18, 16, 241, 135, 1, 18, 17, 193, 135, 112, 211, 135, 17, 124]
    try runningSource(source, expecting: expected)
  }

  func testAtIfAbsent() throws {
    let source = """
at: key ifAbsent: aBlock
	"Answer with the value at key.  If key is not found, evaluate aBlock."

	| index |
	index _ self findKey: key ifAbsent: [^aBlock value].
	^valueArray at: index

"""
    // 5 .. 20
    let expected = [112, 16, 137, 117, 200, 164, 3, 17, 201, 124, 240, 106, 1, 18, 192, 124]
    try runningSource(source, expecting: expected)
  }

  func testRemoveKeyIfAbsent() throws {
    let source = """
removeKey: key ifAbsent: errorBlock
	self removeDangerouslyKey: key ifAbsent: [^errorBlock value]

"""
    // 5 .. 17
    let expected = [112, 16, 137, 117, 200, 164, 3, 17, 201, 124, 240, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAssociationsDo() throws {
    let source = """
associationsDo: aBlock
	"Evaluate aBlock for associations consisting of my keys and their values."

	1 to: self basicSize do:
		[:index |
		(self basicAt: index) == nil
			ifFalse: [aBlock value: (Association
										key: (self basicAt: index)
										value: (valueArray at: index))]]

"""
    // 13 .. 44
    let expected = [118, 112, 209, 137, 118, 200, 164, 21, 105, 112, 17, 228, 115, 198, 154, 115, 164, 10, 16, 67, 112, 17, 228, 1, 17, 192, 242, 202, 125, 240, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAdd() throws {
    let source = """
add: anAssociation
	self at: anAssociation key put: anAssociation value

"""
    // 5 .. 12
    let expected = [112, 16, 208, 16, 201, 193, 135, 120]
    try runningSource(source, expecting: expected)
  }

}
