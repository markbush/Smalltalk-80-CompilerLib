import XCTest
@testable import CompilerLib

final class CompileClassDescriptionTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("ClassDescription", instanceVariables: ["superclass", "methodDict", "format", "subclasses", "instanceVariables", "organization"])
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

  func testPrintMethodChunkOnMoveSourceToFile() throws {
    let source = """
printMethodChunk: aSelector on: aFileStream moveSource: moveSource toFile: fileIndex
	"Print the source code for the method associated with the argument
	selector onto
	the fileStream. aFileStream, and, for backup, if the argument
	moveSource (a Boolean)
	is true, also set the file index within the method to be the argument
	fileIndex. "

	| position |
	aFileStream cr.
	Cursor write showWhile:
		[moveSource
			ifTrue:
				[position _ aFileStream position.
				aFileStream nextChunkPut: (self sourceCodeAt: aSelector).
				(self compiledMethodAt: aSelector)
					setSourcePosition: position inFile: fileIndex]
			ifFalse: [aFileStream cr; nextChunkPut: (self sourceCodeAt: aSelector)]]

"""
    // 21 .. 61
    let expected = [17, 208, 135, 67, 210, 137, 117, 200, 164, 28, 18, 172, 16, 17, 214, 108, 17, 112, 16, 229, 228, 135, 112, 16, 232, 20, 19, 247, 151, 17, 136, 208, 135, 112, 16, 229, 228, 125, 225, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testCopyCategoryFromClassified() throws {
    let source = """
copyCategory: cat from: aClass classified: newCat
	"Specify that one of the categories of messages for the receiver is the third argument,
	newCat.  Copy each message found in the category cat in class aClass into this
	new category."

	self copyAll: (aClass organization listAtCategoryNamed: cat)
		from: aClass
		classified: newCat

"""
    // 9 .. 19
    let expected = [112, 17, 210, 16, 225, 17, 18, 131, 96, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testErrorCategoryName() throws {
    let source = """
errorCategoryName
	self error: 'Category name must be a String'

"""
    // 7 .. 11
    let expected = [112, 33, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testClassVariablesString() throws {
    let source = """
classVariablesString
	"Answer a string of my class variable names separated by spaces, in alphabetical order."
	| aStream |
	aStream _ WriteStream on: (String new: 100).
	self classPool keys asSortedCollection do: [:key | aStream nextPutAll: key; space].
	^ aStream contents

"""
    // 23 .. 50
    let expected = [65, 66, 35, 205, 224, 104, 112, 214, 213, 212, 137, 118, 200, 164, 8, 105, 16, 136, 17, 231, 135, 216, 125, 203, 135, 16, 217, 124]
    try runningSource(source, expecting: expected)
  }

  func testInstanceVariablesString() throws {
    let source = """
instanceVariablesString
	"Answer a string of my instance variable names separated by spaces."
	| aStream names |
	aStream _ WriteStream on: (String new: 100).
	names _ self instVarNames.
	1 to: names size do: [:i | aStream nextPutAll: (names at: i); space].
	^ aStream contents

"""
    // 21 .. 52
    let expected = [65, 66, 35, 205, 224, 104, 112, 212, 105, 118, 17, 194, 137, 118, 200, 164, 10, 106, 16, 136, 17, 18, 192, 230, 135, 215, 125, 245, 135, 16, 216, 124]
    try runningSource(source, expecting: expected)
  }

  func testSubclassOfOldClassInstanceVariableNamesVariableWordsPointersIfBad() throws {
    let source = """
subclassOf: newSuper oldClass: oldClass instanceVariableNames: newInstVarString variable: v words: w pointers: p ifBad: badBlock
	"Basic initialization message for creating classes using the information provided
	as arguments.  Answer whether old instances will be invalidated."

	| old new usedNames invalid oldSuperMeta newInstVarArray |
	old _ self allInstVarNames.
	usedNames _ #(self super thisContext true false nil ) asSet.
	newInstVarArray _ Scanner new scanFieldNames: newInstVarString.
	(invalid _ superclass ~~ newSuper)
		ifTrue:
			["superclass changed"
			oldSuperMeta _ superclass class.
			superclass removeSubclass: self.
			superclass _ newSuper.
			superclass addSubclass: self.
			self class superclass == oldSuperMeta
				ifTrue: ["Only false when self is a metaclass"
						self class superclass: newSuper class]].
	instanceVariables _ nil.  "To give us all super names"
	new _ self allInstVarNames , newInstVarArray.
	new do:
		[:fieldName |
		(usedNames includes: fieldName)
			ifTrue:
				[self error: fieldName , ' is reserved (maybe in a superclass)'.
				^badBlock value].
		usedNames add: fieldName].
	instanceVariables _ newInstVarArray size = 0
		ifTrue: [nil]
		ifFalse: [newInstVarArray].
	invalid _ invalid | (new ~= old).
	"field names changed"
	old _ format.
	self
		format: new size
		variable: v
		words: w
		pointers: p.
	invalid _ invalid | (format ~= old).
	"format changed"
	^invalid

"""
    // 41 .. 158
    let expected = [112, 208, 111, 34, 209, 130, 73, 68, 204, 18, 227, 130, 76, 0, 16, 233, 129, 74, 172, 26, 0, 199, 130, 75, 0, 112, 229, 135, 16, 96, 0, 112, 230, 135, 112, 199, 216, 27, 198, 157, 112, 199, 16, 199, 231, 135, 115, 100, 112, 208, 28, 234, 130, 72, 24, 137, 118, 200, 164, 20, 130, 77, 25, 29, 237, 172, 9, 112, 29, 44, 234, 235, 135, 22, 201, 124, 25, 29, 238, 125, 203, 135, 28, 194, 117, 182, 153, 115, 144, 28, 100, 26, 24, 23, 183, 239, 130, 74, 2, 111, 112, 24, 194, 19, 20, 21, 131, 144, 135, 26, 2, 23, 183, 239, 130, 74, 26, 124]
    try runningSource(source, expecting: expected)
  }

  func testFileOutOn() throws {
    let source = """
fileOutOn: aFileStream
	"File a description of the receiver on aFileStream."

	self fileOutOn: aFileStream
		moveSource: false
		toFile: 0

"""
    // 5 .. 12
    let expected = [112, 16, 114, 117, 131, 96, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testRemoveCategory() throws {
    let source = """
removeCategory: aString
	"Remove each of the messages categorized under aString in the method dictionary
	of the receiver.  Then remove the category aString."

	(self organization listAtCategoryNamed: aString asSymbol) do:
		[:sel | self removeSelector: sel].
	self organization removeEmptyCategories

"""
    // 13 .. 34
    let expected = [112, 209, 16, 210, 224, 137, 118, 200, 164, 5, 105, 112, 17, 227, 125, 203, 135, 112, 209, 212, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testCompileNotifyingTrailerIfFail() throws {
    let source = """
compile: code notifying: requestor trailer: bytes ifFail: failBlock
	"Intercept this message in order to remember system changes."

	| methodNode selector |
	Cursor execute showWhile:
		[methodNode _
			 	self compilerClass new
					compile: code
					in: self
					notifying: requestor
					ifFail: failBlock.
	selector _ methodNode selector.
	(methodDict includesKey: selector)
		ifTrue: [Smalltalk changes changeSelector: selector class: self]
		ifFalse: [Smalltalk changes addSelector: selector class: self].
	self addSelector: selector withMethod: (methodNode generate: bytes)].
	^selector

"""
    // 29 .. 75
    let expected = [66, 209, 137, 117, 200, 164, 36, 112, 212, 204, 16, 112, 17, 19, 131, 131, 108, 20, 213, 109, 1, 21, 234, 157, 72, 215, 21, 112, 249, 148, 72, 215, 21, 112, 246, 135, 112, 21, 20, 18, 236, 251, 125, 224, 135, 21, 124]
    try runningSource(source, expecting: expected)
  }

  func testLogOrganizationChange() throws {
    let source = """
logOrganizationChange
	"Record that the receiver is being reorganized on the changes file."
	| file |
	SourceFiles == nil
		ifFalse:
			[file _ SourceFiles at: 2.
			file setToEnd; readWriteShorten.
			file cr; nextChunkPut:
				self name, ' organization changeFromString: ',
					self organization printString storeString.
			file cr; readOnly]

"""
    // 27 .. 63
    let expected = [64, 115, 198, 168, 31, 64, 119, 192, 104, 16, 136, 209, 135, 210, 135, 16, 136, 211, 135, 112, 214, 39, 229, 112, 218, 217, 216, 229, 228, 135, 16, 136, 211, 135, 219, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testIsMeta() throws {
    let source = """
isMeta
	^ false

"""
    // 3 .. 3
    let expected = [122]
    try runningSource(source, expecting: expected)
  }

  func testSharedPoolsString() throws {
    let source = """
sharedPoolsString
	"Answer a string of my class variable names separated by spaces."
	| aStream |
	aStream _ WriteStream on: (String new: 100).
	self sharedPools do: [:x | aStream nextPutAll: (Smalltalk keyAtValue: x); space].
	^ aStream contents

"""
    // 23 .. 50
    let expected = [65, 66, 35, 205, 224, 104, 112, 212, 137, 118, 200, 164, 10, 105, 16, 136, 71, 17, 230, 229, 135, 216, 125, 203, 135, 16, 217, 124]
    try runningSource(source, expecting: expected)
  }

  func testStoreOn() throws {
    let source = """
storeOn: aStream
	"Clases and Metaclasses have global names."
	aStream nextPutAll: self name

"""
    // 7 .. 12
    let expected = [16, 112, 209, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testName() throws {
    let source = """
name
	"Answer a String that is the name of the receiver."

	self subclassResponsibility

"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testSetCategory() throws {
    let source = """
category: cat
	"Categorize the receiver under the system category, cat, removing it from any
	previous categorization."

	(cat isKindOf: String)
		ifTrue: [SystemOrganization classify: self name under: cat asSymbol]
		ifFalse: [self errorCategoryName]

"""
    // 17 .. 31
    let expected = [16, 70, 229, 158, 66, 112, 211, 16, 212, 241, 145, 112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testSetComment() throws {
    let source = """
comment: aString
	"Set the receiver's comment to be the argument, aString."

	| aStream |
	aString size = 0
		ifTrue:
			[self organization classComment: aString]
		ifFalse:
			["double internal quotes of the comment string"
			aStream _ WriteStream on: (String new: aString size).
			aStream nextPutAll: self name , ' comment:'; cr.
			aString storeOn: aStream.
			self organization classComment: aStream contents.
	Smalltalk changes commentClass: self]

"""
    // 33 .. 76
    let expected = [16, 194, 117, 182, 157, 112, 218, 16, 233, 164, 31, 65, 66, 16, 194, 205, 224, 105, 17, 136, 112, 213, 38, 228, 227, 135, 215, 135, 16, 17, 232, 135, 112, 218, 17, 219, 233, 135, 78, 221, 112, 236, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testObsolete() throws {
    let source = """
obsolete
	"Make the receiver obsolete."

	organization _ nil.
	super obsolete

"""
    // 7 .. 13
    let expected = [115, 101, 112, 133, 0, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testFileOutCategory() throws {
    let source = """
fileOutCategory: aString
	"Create a file whose name is the name of the receiver with -.st- as the
	extension, and file a description of the receiver's category aString onto it"

	| fileName fileStream |
	fileName _ Disk checkName: self name , '-' , aString , '.st' fixErrors: true.
	fileStream _ Disk file: fileName.
	fileStream timeStamp.
	self fileOutCategory: aString
		on: fileStream
		moveSource: false
		toFile: 0.
	fileStream shorten; close

"""
    // 25 .. 58
    let expected = [65, 112, 211, 36, 226, 16, 226, 37, 226, 113, 240, 105, 65, 17, 230, 106, 18, 215, 135, 112, 16, 18, 114, 117, 131, 136, 135, 18, 136, 217, 135, 218, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testPrintOutCategory() throws {
    let source = """
printOutCategory: aString
	"Create a readable version of the message category aString, and send to a printer.
	Defaults to fileOut."
	self fileOutCategory: aString

"""
    // 5 .. 9
    let expected = [112, 16, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testFileOutChangedMessagesOn() throws {
    let source = """
fileOutChangedMessages: aSet on: aFileStream
	"File a description of the messages of the receiver that have been changed
	(i.e., are entered into the system ChangeSet) onto aFileStream."

	self fileOutChangedMessages: aSet
		on: aFileStream
		moveSource: false
		toFile: 0

"""
    // 5 .. 13
    let expected = [112, 16, 17, 114, 117, 131, 128, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testFileOutOnMoveSourceToFile() throws {
    let source = """
fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex
	"file me out on aFileStream"
	aFileStream emphasis: 5.		"Meant to be 12 point bold font."
	aFileStream nextChunkPut: self definition.
	self organization
		putCommentOnFile: aFileStream
		numbered: fileIndex
		moveSource: moveSource.
	aFileStream cr.
	self organization categories do:
		[:heading |
		self
			fileOutCategory: heading
			on: aFileStream
			moveSource: moveSource
			toFile: fileIndex]

"""
    // 21 .. 60
    let expected = [16, 33, 224, 135, 16, 112, 211, 226, 135, 112, 213, 16, 18, 17, 131, 100, 135, 16, 214, 135, 112, 213, 215, 137, 118, 200, 164, 9, 107, 112, 19, 16, 17, 18, 131, 136, 125, 203, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testRemoveSelector() throws {
    let source = """
removeSelector: aSymbol
	"Remove the message whose selector is aSymbol from the method
	dictionary of the receiver, if it is there.  Answer nil otherwise."

	(methodDict includesKey: aSymbol) ifFalse: [^nil].
	super removeSelector: aSymbol.
	self organization removeElement: aSymbol.
	Smalltalk changes removeSelector: aSymbol class: self.
	Smalltalk logChange: self name , ' removeSelector: #' , aSymbol

"""
    // 27 .. 58
    let expected = [1, 16, 224, 168, 1, 123, 112, 16, 133, 33, 135, 112, 211, 16, 226, 135, 70, 213, 16, 112, 244, 135, 70, 112, 217, 42, 232, 16, 232, 231, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testComment() throws {
    let source = """
comment
	"Answer the receiver's comment."

	| aString |
	aString _ self organization classComment.
	aString size = 0 ifTrue: [^''].
	"get string only of classComment, undoubling quotes"
	^ String readFromString: aString

"""
    // 13 .. 27
    let expected = [112, 209, 208, 104, 16, 194, 117, 182, 153, 34, 124, 68, 16, 227, 124]
    try runningSource(source, expecting: expected)
  }

  func testCompileClassified() throws {
    let source = """
compile: code classified: heading
	"Compile the argument, code, as source code in the context of the receiver and
	install the result in the receiver's method dictionary under the classification
	indicated by the second argument, heading. nil is to be notified if an error occurs.
	The argument code is either a string or an object that converts to a string or a
	PositionableStream on an object that converts to a string."

	^self
		compile: code
		classified: heading
		notifying: nil

"""
    // 5 .. 11
    let expected = [112, 16, 17, 115, 131, 96, 124]
    try runningSource(source, expecting: expected)
  }

  func testDefinition() throws {
    let source = """
definition
	"Answer a string that defines the receiver."
	| aStream |
	aStream _ WriteStream on: (String new: 300).
	self hasMultipleSuperclasses
		ifTrue:
			[aStream nextPutAll: 'Class named: '.
			self name storeOn: aStream.
			aStream cr; tab; nextPutAll: 'superclasses: '.
			aStream store: self superclassesString.
			aStream cr; tab; nextPutAll: 'instanceVariableNames: '.
			aStream store: self instanceVariablesString.
			aStream cr; tab; nextPutAll: 'classVariableNames: '.
			aStream store: self classVariablesString]
		ifFalse:
			[aStream nextPutAll: (superclass == nil ifTrue: ['nil'] ifFalse: [superclass name]).
			aStream nextPutAll: self kindOfSubclass.
			self name storeOn: aStream.
			aStream cr; tab; nextPutAll: 'instanceVariableNames: '.
			aStream store: self instanceVariablesString.
			aStream cr; tab; nextPutAll: 'classVariableNames: '.
			aStream store: self classVariablesString.
			aStream cr; tab; nextPutAll: 'poolDictionaries: '.
			aStream store: self sharedPoolsString].
	aStream cr; tab; nextPutAll: 'category: '.
	(SystemOrganization categoryOfElement: self name) asString storeOn: aStream.
	^aStream contents

"""
    // 57 .. 214
    let expected = [65, 66, 35, 205, 224, 104, 112, 131, 21, 172, 56, 16, 50, 228, 135, 112, 213, 16, 232, 135, 16, 136, 217, 135, 136, 218, 135, 51, 228, 135, 16, 112, 131, 20, 236, 135, 16, 136, 217, 135, 136, 218, 135, 43, 228, 135, 16, 112, 221, 236, 135, 16, 136, 217, 135, 136, 218, 135, 46, 228, 135, 16, 112, 223, 236, 164, 66, 16, 0, 115, 198, 153, 38, 145, 0, 213, 228, 135, 16, 112, 215, 228, 135, 112, 213, 16, 232, 135, 16, 136, 217, 135, 136, 218, 135, 43, 228, 135, 16, 112, 221, 236, 135, 16, 136, 217, 135, 136, 218, 135, 46, 228, 135, 16, 112, 223, 236, 135, 16, 136, 217, 135, 136, 218, 135, 48, 228, 135, 16, 112, 131, 17, 236, 135, 16, 136, 217, 135, 136, 218, 135, 54, 228, 135, 89, 112, 213, 131, 56, 131, 23, 16, 232, 135, 16, 131, 26, 124]
    try runningSource(source, expecting: expected)
  }

  func testCategory() throws {
    let source = """
category
	"Answer the system organization category for the receiver."

	^SystemOrganization categoryOfElement: self name

"""
    // 9 .. 13
    let expected = [65, 112, 210, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testOrganization() throws {
    let source = """
organization
	"Answer the instance of ClassOrganizer that represents the organization
	of the messages of the receiver."
	organization==nil
		ifTrue: [organization _ ClassOrganizer new].
	^organization

"""
    // 5 .. 13
    let expected = [5, 115, 198, 154, 64, 204, 101, 5, 124]
    try runningSource(source, expecting: expected)
  }

  func testReorganize() throws {
    let source = """
reorganize
	"Record that the receiver is being reorganized and answer the receiver's organization."

	Smalltalk changes reorganizeClass: self.
	^self organization

"""
    // 11 .. 18
    let expected = [66, 209, 112, 224, 135, 112, 211, 124]
    try runningSource(source, expecting: expected)
  }

  func testInstVarNames() throws {
    let source = """
instVarNames
	"Answer an Array of the names of instance variables defined in the receiver."

	instanceVariables == nil
		ifTrue: [^#()]
		ifFalse: [^instanceVariables]

"""
    // 5 .. 12
    let expected = [4, 115, 198, 153, 32, 124, 4, 124]
    try runningSource(source, expecting: expected)
  }

  func testFileOutOrganizationOn() throws {
    let source = """
fileOutOrganizationOn: aFileStream
	"File a description of the receiver's organization onto aFileStream."

	aFileStream emphasis: 3.
	aFileStream cr; nextPut: $!.
	aFileStream nextChunkPut: self name, ' reorganize'; cr.
	aFileStream nextChunkPut: self organization printString; cr.
	aFileStream emphasis: 1

"""
    // 23 .. 57
    let expected = [16, 33, 224, 135, 16, 136, 210, 135, 35, 196, 135, 16, 136, 112, 214, 39, 229, 228, 135, 210, 135, 16, 136, 112, 217, 216, 228, 135, 210, 135, 16, 118, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testWhichCategoryIncludesSelector() throws {
    let source = """
whichCategoryIncludesSelector: aSelector
	"Answer the category of the argument, aSelector, in the organization of the
	receiver, or answer nil if the receiver does not inlcude this selector."

	(self includesSelector: aSelector)
		ifTrue: [^organization categoryOfElement: aSelector]
		ifFalse: [^nil]

"""
    // 7 .. 15
    let expected = [112, 16, 225, 155, 5, 16, 224, 124, 123]
    try runningSource(source, expecting: expected)
  }

  func testCompileClassifiedNotifying() throws {
    let source = """
compile: code classified: heading notifying: requestor
	"Compile the argument, code, as source code in the context of the receiver and
	install the result in the receiver's method dictionary under the classification
	indicated by the second argument, heading  The third argument,
	requestor, is to be notified if an error occurs. The argument code is either a string or
	an object that converts to a string or a PositionableStream on an object that converts
	to a string."

	| selector |
	selector _
		self compile: code
			notifying: requestor
			trailer: #(0 0 0 )
			ifFail: [^nil].
	(methodDict at: selector)
		putSource: code asString
		class: self
		category: heading
		inFile: 2.
	self organization classify: selector under: heading.
	^selector

"""
    // 15 .. 46
    let expected = [112, 16, 18, 33, 137, 117, 200, 164, 1, 123, 131, 128, 107, 1, 19, 192, 16, 211, 112, 17, 119, 131, 130, 135, 112, 213, 19, 17, 244, 135, 19, 124]
    try runningSource(source, expecting: expected)
  }

  func testCommentTemplate() throws {
    let source = """
commentTemplate
	"Answer an expression to edit and evaluate in order to produce the receiver's comment."

	| aString |
	aString _ self organization classComment.
	aString size = 0
		ifTrue: [^self name , ' comment:
''This class has not yet been commented.  A proper comment should include the purpose of the class and the type and purpose of each instance variable.
''']
		ifFalse: [^aString]

"""
    // 13 .. 28
    let expected = [112, 209, 208, 104, 16, 194, 117, 182, 156, 112, 211, 36, 226, 124, 16, 124]
    try runningSource(source, expecting: expected)
  }

  func testValidateFromInInstanceVariableNamesMethods() throws {
    let source = """
validateFrom: oldClass in: environ instanceVariableNames: invalidFields methods: invalidMethods
	"Recompile the receiver, a class, and redefine its subclasses if necessary."

	| sub newSub |
	invalidFields & invalidMethods ifFalse: [^self].
	invalidMethods & self hasMethods
		ifTrue:
			[Transcript show: 'recompiling ' , self name , '...'.
			self compileAllFrom: oldClass.
			Transcript show: ' done'; cr].
	self ~~ oldClass ifTrue: [self updateInstancesFrom: oldClass].
	oldClass subclasses do:
		[:sub |
		newSub _ sub copyForValidation.
		newSub
			subclassOf: self
			oldClass: sub
			instanceVariableNames: sub instVarNames
			variable: sub isVariable
			words: sub isBytes not
			pointers: sub isBits not
			ifBad: [self error: 'terrible problem in recompiling subclasses!'].
		newSub
			validateFrom: sub
			in: environ
			instanceVariableNames: invalidFields
			methods: invalidMethods]

"""
    // 51 .. 144
    let expected = [18, 19, 224, 168, 1, 120, 19, 112, 218, 224, 172, 20, 66, 36, 112, 213, 227, 38, 227, 225, 135, 112, 16, 231, 135, 66, 136, 40, 225, 135, 217, 135, 112, 16, 236, 155, 112, 16, 235, 135, 16, 221, 137, 118, 200, 164, 44, 108, 20, 222, 109, 21, 112, 20, 20, 131, 16, 20, 131, 17, 20, 131, 19, 131, 18, 20, 131, 20, 131, 18, 137, 117, 200, 164, 5, 112, 54, 131, 53, 125, 131, 239, 135, 21, 20, 17, 18, 19, 131, 151, 125, 203, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testMethodsFor() throws {
    let source = """
methodsFor: aString
	"Answer a ClassCategoryReader for accessing the messages in the method
	dictionary category, aString, of the receiver."

	^ClassCategoryReader class: self category: aString asSymbol
	"False methodsFor: 'logical operations' inspect"

"""
    // 9 .. 14
    let expected = [65, 112, 16, 210, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testCopyCategoryFrom() throws {
    let source = """
copyCategory: cat from: class
	"Specify that one of the categories of messages for the receiver is cat, as found
	in the class, aClass.  Copy each message found in this category."

	self copyCategory: cat
		from: class
		classified: cat

"""
    // 5 .. 12
    let expected = [112, 16, 17, 16, 131, 96, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testCopyAllFromClassified() throws {
    let source = """
copyAll: selArray from: class classified: cat
	"Install all the methods found in the method dictionary of the second argument, class,
	as the receiver's methods.  Classify the messages under the third argument, cat."

	selArray do:
		[:s | self copy: s
				from: class
				classified: cat]

"""
    // 5 .. 21
    let expected = [16, 137, 118, 200, 164, 8, 107, 112, 19, 17, 18, 131, 96, 125, 203, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testPrintOn() throws {
    let source = """
printOn: aStream
	aStream nextPutAll: self name

"""
    // 7 .. 12
    let expected = [16, 112, 209, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testCopyAllFrom() throws {
    let source = """
copyAll: selArray from: class
	"Install all the methods found in the method dictionary of the second argument, class,
	as the receiver's methods.  Classify the messages under -as yet not classified-"

	self copyAll: selArray
		from: class
		classified: nil

"""
    // 5 .. 12
    let expected = [112, 16, 17, 115, 131, 96, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testFileOutCategoryOnMoveSourceToFile() throws {
    let source = """
fileOutCategory: aString on: aFileStream moveSource: moveSource toFile: fileIndex
	"File a description of the receiver's category, aString, onto aFileStream.  If
	the boolean argument, moveSource, is true, then set the trailing bytes to the position
	of aFileStream and to fileIndex in order to indicate where to find the source code."

	self printCategoryChunk: aString on: aFileStream.
	(self organization listAtCategoryNamed: aString)
		do: [:sel | self
				printMethodChunk: sel
				on: aFileStream
				moveSource: moveSource
				toFile: fileIndex].
	aFileStream nextChunkPut: ' '

"""
    // 15 .. 44
    let expected = [112, 16, 17, 240, 135, 112, 210, 16, 225, 137, 118, 200, 164, 9, 108, 112, 20, 17, 18, 19, 131, 131, 125, 203, 135, 17, 37, 228, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testAddInstVarName() throws {
    let source = """
addInstVarName: aString
	"Add the argument, aString, as one of the receiver's instance variables."

	self subclassResponsibility

"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testRemoveInstVarName() throws {
    let source = """
removeInstVarName: aString
	"Remove the argument, aString, as one of the receiver's instance variables."

	self subclassResponsibility

"""
    // 5 .. 8
    let expected = [112, 208, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testCopyAllCategoriesFrom() throws {
    let source = """
copyAllCategoriesFrom: aClass
	"Specify that the categories of messages for the receiver include all of those found
	in the class, aClass.  Install each of the messages found in these categories into the
	method dictionary of the receiver, classified under the appropriate categories."

	aClass organization categories do: [:cat | self copyCategory: cat from: aClass]

"""
    // 9 .. 25
    let expected = [16, 209, 208, 137, 118, 200, 164, 6, 105, 112, 17, 16, 242, 125, 203, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testUpdateInstancesFrom() throws {
    let source = """
updateInstancesFrom: oldClass
	"Recreate any existing instances of the argument, oldClass, as
	instances of the receiver, which is a newly changed class.  Permute variables
	as necessary."

	| oldInstVarNames map variable old new instSize offset fieldName |
	oldClass someInstance == nil ifTrue: [^self].
	"no instances to convert"
	oldInstVarNames _ oldClass allInstVarNames.
	map _
		self allInstVarNames
			collect: [:instVarName | oldInstVarNames indexOf: instVarName].
	variable _ self isVariable.
	instSize _ self instSize.
	oldClass allInstances do:
		[:old |
		"note allInstsDo would get confused by becoming"
		variable
			ifTrue: [new _ self basicNew: old basicSize]
			ifFalse: [new _ self basicNew].
		1 to: instSize do:
			[:offset |
			(map at: offset) > 0
				ifTrue: [new instVarAt: offset put: (old instVarAt: (map at: offset))]].
		variable
			ifTrue: [1 to: old basicSize do:
						[:offset | new basicAt: offset put: (old basicAt: offset)]].
		old become: new]

"""
    // 35 .. 142
    let expected = [16, 208, 115, 198, 152, 120, 16, 209, 105, 112, 209, 137, 118, 200, 164, 6, 130, 73, 17, 25, 227, 125, 226, 106, 112, 212, 107, 112, 213, 110, 16, 214, 137, 118, 200, 164, 68, 108, 19, 158, 112, 20, 217, 232, 129, 69, 147, 112, 215, 129, 69, 135, 118, 22, 137, 118, 200, 164, 19, 111, 18, 23, 192, 117, 179, 172, 9, 21, 23, 20, 18, 23, 192, 236, 251, 144, 115, 125, 250, 135, 19, 172, 18, 118, 20, 217, 137, 118, 200, 164, 8, 111, 21, 23, 20, 23, 238, 253, 125, 250, 135, 20, 21, 239, 125, 203, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testCopyFrom() throws {
    let source = """
copy: sel from: class
	"Install the method associated with the first arugment, sel, a message selector,
	found in the method dictionary of the second argument, class, as one of the
	receiver's methods.  Classify the message under -as yet not classified-"

	self copy: sel
		from: class
		classified: nil

"""
    // 5 .. 12
    let expected = [112, 16, 17, 115, 131, 96, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testSuperclassesString() throws {
    let source = """
superclassesString
	"Answer a string of my superclass names separated by spaces."
	| aStream names |
	aStream _ WriteStream on: (String new: 100).
	self superclasses do: [:each | aStream nextPutAll: each name; space].
	^ aStream contents

"""
    // 21 .. 47
    let expected = [65, 66, 35, 205, 224, 104, 112, 212, 137, 118, 200, 164, 9, 106, 16, 136, 18, 214, 229, 135, 215, 125, 203, 135, 16, 216, 124]
    try runningSource(source, expecting: expected)
  }

  func testFileOutMessageOnMoveSourceToFile() throws {
    let source = """
fileOutMessage: aString on: aFileStream moveSource: moveSource toFile: fileIndex
	"File a description of the receiver's message, aString, onto aFileStream.  If
	the boolean argument, moveSource, is true, then set the trailing bytes to the position
	of aFileStream and to fileIndex in order to indicate where to find the source code."

	| cat |
	cat _ self organization categoryOfElement: aString.
	cat == nil ifTrue: [^self error: 'no such message'].
	self printCategoryChunk: cat on: aFileStream.
	self
		printMethodChunk: aString
		on: aFileStream
		moveSource: moveSource
		toFile: fileIndex.
	aFileStream nextChunkPut: ' '

"""
    // 19 .. 49
    let expected = [112, 209, 16, 224, 108, 20, 115, 198, 155, 112, 35, 226, 124, 112, 20, 17, 244, 135, 112, 16, 17, 18, 19, 131, 133, 135, 17, 39, 230, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testFileOutMessageFileName() throws {
    let source = """
fileOutMessage: aString fileName: fileName
	"Create a local file named fileName
	and file a description of the receiver's message aString onto it"

	| fileStream |
	fileStream _ Disk file: fileName.
	fileStream timeStamp.
	self fileOutMessage: aString
		on: fileStream
		moveSource: false
		toFile: 0.
	fileStream close

"""
    // 13 .. 31
    let expected = [65, 17, 224, 106, 18, 210, 135, 112, 16, 18, 114, 117, 131, 131, 135, 18, 212, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testFileOutMessage() throws {
    let source = """
fileOutMessage: aString
	"Create a fileName which is the name of the receiver with -.st as the
	extension, and file a description of the receiver's message aString onto it"

	self fileOutMessage: aString
		fileName: (Disk checkName: self name , '-' , aString , '.st' fixErrors: true)

"""
    // 17 .. 32
    let expected = [112, 16, 66, 112, 212, 37, 227, 16, 227, 38, 227, 113, 241, 240, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testPrintOutMessage() throws {
    let source = """
printOutMessage: aString
	"Create a readable version of the message with selector aString, and send to a printer.
	Defaults to fileOut."
	self fileOutMessage: aString

"""
    // 5 .. 9
    let expected = [112, 16, 224, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testMoveChangesTo() throws {
    let source = """
moveChangesTo: newFile
	"Used in the process of condensing changes, this message requests that the source
	code of all methods of the receiver that have been changed should be moved to
	newFile."

	| changes |
	self organization moveChangedCommentToFile: newFile numbered: 2.
	changes _ methodDict keys select: [:sel | (methodDict at: sel) fileIndex > 1].
	self fileOutChangedMessages: changes
		on: newFile
		moveSource: true
		toFile: 2

"""
    // 15 .. 46
    let expected = [112, 209, 16, 119, 240, 135, 1, 211, 137, 118, 200, 164, 8, 106, 1, 18, 192, 212, 118, 179, 125, 226, 105, 112, 17, 16, 113, 119, 131, 133, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testCopyFromClassified() throws {
    let source = """
copy: sel from: class classified: cat
	"Install the method associated with the first arugment, sel, a message selector,
	found in the method dictionary of the second argument, class, as one of the
	receiver's methods.  Classify the message under the third argument, cat."

	| code category |
	"Useful when modifying an existing class"
	code _ class sourceMethodAt: sel.
	code == nil
		ifFalse:
			[cat == nil
				ifTrue: [category _ class organization categoryOfElement: sel]
				ifFalse: [category _ cat].
			(methodDict includesKey: sel)
				ifTrue: [code asString = (self sourceMethodAt: sel) asString
							ifFalse: [self error: self name
										, ' '
										, sel
										, ' will be redefined if you proceed.']].
			self compile: code classified: category]

"""
    // 25 .. 79
    let expected = [17, 16, 224, 107, 19, 115, 198, 168, 45, 18, 115, 198, 158, 17, 210, 16, 225, 129, 68, 146, 18, 129, 68, 135, 1, 16, 233, 172, 20, 19, 216, 112, 16, 224, 216, 182, 168, 11, 112, 112, 213, 38, 228, 16, 228, 39, 228, 227, 135, 112, 19, 20, 250, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testFileOutChangedMessagesOnMoveSourceToFile() throws {
    let source = """
fileOutChangedMessages: aSet on: aFileStream moveSource: moveSource toFile: fileIndex
	"File a description of the messages of the receiver that have been changed
	(i.e., are entered into the system ChangeSet) onto aFileStream.  If
	the boolean argument, moveSource, is true, then set the trailing bytes to the position
	of aFileStream and to fileIndex in order to indicate where to find the source code."

	| org sels |
	(org _ self organization) categories do:
		[:cat |
		sels _ (org listAtCategoryNamed: cat) select: [:sel | aSet includes: sel].
		sels size > 0
			ifTrue:
				[Transcript cr; show: self name , '>' , cat.
				self printCategoryChunk: cat on: aFileStream.
				sels do: [:sel |
						self
							printMethodChunk: sel
							on: aFileStream
							moveSource: moveSource
							toFile: fileIndex].
				aFileStream nextChunkPut: ' ']]

"""
    // 33 .. 107
    let expected = [112, 209, 129, 68, 208, 137, 118, 200, 164, 62, 110, 20, 22, 227, 137, 118, 200, 164, 5, 111, 16, 23, 228, 125, 226, 109, 21, 194, 117, 179, 172, 38, 69, 136, 214, 135, 112, 217, 42, 232, 22, 232, 231, 135, 112, 22, 17, 251, 135, 21, 137, 118, 200, 164, 9, 111, 112, 23, 17, 18, 19, 131, 140, 125, 203, 135, 17, 46, 237, 144, 115, 125, 203, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testPrintCategoryChunkOn() throws {
    let source = """
printCategoryChunk: aString on: aFileStream
	"print category definition on aFileStream"

	aFileStream cr; cr; nextPut: $!.
	aFileStream nextChunkPut:
				self name , ' methodsFor: ' , '''' , aString , ''''

"""
    // 17 .. 40
    let expected = [17, 136, 208, 135, 136, 208, 135, 33, 196, 135, 17, 112, 212, 37, 227, 38, 227, 16, 227, 38, 227, 226, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testKindOfSubclass() throws {
    let source = """
kindOfSubclass
	"Answer a string that describes what kind of subclass the receiver is, i.e.,
	variable, variable byte, variable word, or not variable."

	self isVariable
		ifTrue: [self isBits
					ifTrue: [self isBytes
								ifTrue: [^' variableByteSubclass: ']
								ifFalse: [^' variableWordSubclass: ']]
					ifFalse: [^' variableSubclass: ']]
		ifFalse: [^' subclass: ']

"""
    // 17 .. 34
    let expected = [112, 214, 172, 12, 112, 213, 158, 112, 212, 153, 35, 124, 34, 124, 33, 124, 32, 124]
    try runningSource(source, expecting: expected)
  }

}
