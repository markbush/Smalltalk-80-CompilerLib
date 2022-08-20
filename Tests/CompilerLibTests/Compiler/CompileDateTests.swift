import XCTest
@testable import CompilerLib

final class CompileDateTests: XCTestCase {
  private var compiler: Compiler!

  override func setUp() {
    super.setUp()
    let classDescription = ClassDescription("Date", instanceVariables: ["day", "year"])
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
= aDate
	"Answer whether aDate is the same day as the receiver."

	self species = aDate species
		ifTrue: [^day = aDate day & (year = aDate year)]
		ifFalse: [^false]

"""
    // 11 .. 28
    let expected = [112, 211, 16, 211, 182, 172, 10, 0, 16, 209, 182, 1, 16, 210, 182, 224, 124, 122]
    try runningSource(source, expecting: expected)
  }

  func testSubtractDate() throws {
    let source = """
subtractDate: aDate
	"Answer the number of days between the receiver and aDate."

	year = aDate year
		ifTrue: [^day - aDate day]
		ifFalse: [^year - 1 // 4 - (aDate year // 4) + day
						+ aDate daysLeftInYear + (year - 1 - aDate year * 365)]

"""
    // 13 .. 47
    let expected = [1, 16, 209, 182, 156, 0, 16, 212, 177, 124, 1, 118, 177, 32, 189, 16, 209, 32, 189, 177, 0, 176, 16, 210, 176, 1, 118, 177, 16, 209, 177, 35, 184, 176, 124]
    try runningSource(source, expecting: expected)
  }

  func testSubtractDays() throws {
    let source = """
subtractDays: dayCount
	"Answer a new Date that is dayCount days before the receiver."

	^Date newDay: day - dayCount year: year

"""
    // 7 .. 13
    let expected = [65, 0, 16, 177, 1, 240, 124]
    try runningSource(source, expecting: expected)
  }

  func testDayOfMonth() throws {
    let source = """
dayOfMonth
	"Answer which day of the month is represented by the receiver."

	^day - (self firstDayOfMonthIndex: self monthIndex) + 1

"""
    // 7 .. 15
    let expected = [0, 112, 112, 209, 224, 177, 118, 176, 124]
    try runningSource(source, expecting: expected)
  }

  func testMonthName() throws {
    let source = """
monthName
	"Answer the name of the month in which the receiver falls."

	^MonthNames at: self monthIndex

"""
    // 7 .. 11
    let expected = [64, 112, 209, 192, 124]
    try runningSource(source, expecting: expected)
  }

  func testWeekdayIndex() throws {
    let source = """
weekdayIndex
	"Sunday=1, ... , Saturday=7"

	| yearIndex dayIndex |
	day < (self firstDayOfMonthIndex: 3)
		ifTrue:
			[yearIndex _ year - 1.
			dayIndex _ 307]
		ifFalse:
			[yearIndex _ year.
			dayIndex _ -58 - self leap].

	^dayIndex + day + yearIndex + (yearIndex // 4)
				+ (yearIndex // 400) - (yearIndex // 100) \\\\ 7 + 1

"""
    // 21 .. 65
    let expected = [0, 112, 36, 227, 178, 159, 1, 118, 177, 104, 34, 129, 65, 151, 1, 104, 32, 112, 209, 177, 129, 65, 135, 17, 0, 176, 16, 176, 16, 37, 189, 176, 16, 38, 189, 176, 16, 39, 189, 177, 40, 186, 118, 176, 124]
    try runningSource(source, expecting: expected)
  }

  func testAsSeconds() throws {
    let source = """
asSeconds
	"Answer the seconds between a time on1 January 1901 and the same time
	in the receiver's day."

	^SecondsInDay * (self subtractDate: (Date newDay: 1 year: 1901))

"""
    // 13 .. 21
    let expected = [64, 112, 67, 118, 36, 242, 225, 184, 124]
    try runningSource(source, expecting: expected)
  }

  func testPrintFormat() throws {
    let source = """
printFormat: formatArray
	"Answer a string description of the receiver.  The argument
	formatArray is the print format, where
	1-3	positions to print day,month,year respectively
	4	character separator
	5	month format (1 month #, 2 first 3 chars, 3 entire name)
	6	year format (1 year #, 2 year # \\\\ 100)"

	| aStream |
	aStream _ WriteStream on: (String new: 16).
	self printOn: aStream format: formatArray.
	^aStream contents

"""
    // 15 .. 28
    let expected = [65, 66, 35, 205, 224, 105, 112, 17, 16, 244, 135, 17, 213, 124]
    try runningSource(source, expecting: expected)
  }

  func testFirstDayOfMonthIndex() throws {
    let source = """
firstDayOfMonthIndex: monthIndex
	"Answer the day of the year (an Integer) that is the first day of my month"

	^(FirstDayOfMonth at: monthIndex)
		+ (monthIndex > 2
				ifTrue: [self leap]
				ifFalse: [0])

"""
    // 7 .. 19
    let expected = [64, 16, 192, 16, 119, 179, 154, 112, 209, 144, 117, 176, 124]
    try runningSource(source, expecting: expected)
  }

  func testDaysInMonth() throws {
    let source = """
daysInMonth
	"Answer the number of days in the month represented by the receiver."

	^(DaysInMonth at: self monthIndex)
		+ (self monthIndex = 2
				ifTrue: [self leap]
				ifFalse: [0])

"""
    // 9 .. 23
    let expected = [64, 112, 209, 192, 112, 209, 119, 182, 154, 112, 210, 144, 117, 176, 124]
    try runningSource(source, expecting: expected)
  }

  func testFirstDayOfMonth() throws {
    let source = """
firstDayOfMonth
	"Answer the index of the day of the year that is the first day
	of the receiver's month"

	^self firstDayOfMonthIndex: self monthIndex

"""
    // 7 .. 11
    let expected = [112, 112, 209, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testDaysInYear() throws {
    let source = """
daysInYear
	"Answer the number of days in the year represented by the receiver."

	^Date daysInYear: self year

"""
    // 9 .. 13
    let expected = [65, 112, 210, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testDaysLeftInYear() throws {
    let source = """
daysLeftInYear
	"Answer the number of days in the year after the date of the receiver."

	^self daysInYear - self day

"""
    // 7 .. 12
    let expected = [112, 208, 112, 209, 177, 124]
    try runningSource(source, expecting: expected)
  }

  func testWeekday() throws {
    let source = """
weekday
	"Answer the name of the day of the week on which the receiver falls."

	^WeekDayNames at: self weekdayIndex

"""
    // 7 .. 11
    let expected = [64, 112, 209, 192, 124]
    try runningSource(source, expecting: expected)
  }

  func testAddDays() throws {
    let source = """
addDays: dayCount
	"Answer a new Date that is dayCount more days than the receiver."

	^Date newDay: day + dayCount
		  year: year

"""
    // 7 .. 13
    let expected = [65, 0, 16, 176, 1, 240, 124]
    try runningSource(source, expecting: expected)
  }


  func testPrevious() throws {
    let source = """
previous: dayName
	"Answer the previous date whose weekday name is dayName."

	^self subtractDays: 7 + self weekdayIndex - (Date dayOfWeek: dayName) \\\\ 7

"""
    // 13 .. 25
    let expected = [112, 33, 112, 210, 176, 68, 16, 227, 177, 33, 186, 224, 124]
    try runningSource(source, expecting: expected)
  }


  func testLeap() throws {
    let source = """
leap
	"Answer whether the receiver's year is a leap year."

	^Date leapYear: year

"""
    // 7 .. 10
    let expected = [65, 1, 224, 124]
    try runningSource(source, expecting: expected)
  }

  func testPrintOnFormat() throws {
    let source = """
printOn: aStream format: formatArray
	"Print a description of the receiver on aStream.  The argument
	formatArray is the print format, where
	1-3	positions to print day,month,year respectively
	4	character separator
	5	month format (1 month #, 2 first 3 chars, 3 entire name)
	6	year format (1 year #, 2 year # \\\\ 100)"

	| monthIndex element monthFormat |
	monthIndex _ self monthIndex.
	1 to: 3 do:
		[:elementIndex |
		element _ formatArray at: elementIndex.
		element = 1 ifTrue: [day - self firstDayOfMonth + 1 printOn: aStream].
		element = 2
			ifTrue:
				[monthFormat _ formatArray at: 5.
				monthFormat = 1
					ifTrue: [monthIndex printOn: aStream].
				monthFormat = 2
					ifTrue: [aStream nextPutAll: ((MonthNames at: monthIndex)
													copyFrom: 1 to: 3)].
				monthFormat = 3
					ifTrue: [aStream nextPutAll: (MonthNames at: monthIndex)]].
		element = 3
			ifTrue:
				[(formatArray at: 6) = 1
					ifTrue: [year printOn: aStream]
					ifFalse: [(year \\\\ 100) printOn: aStream]].
		elementIndex < 3
			ifTrue:
				[(formatArray at: 4) ~= 0
					ifTrue: [aStream nextPut: (formatArray at: 4) asCharacter]]]

"""
    // 29 .. 144
    let expected = [112, 208, 106, 118, 34, 137, 118, 200, 164, 103, 109, 17, 21, 192, 107, 19, 118, 182, 172, 9, 0, 112, 212, 177, 118, 176, 16, 227, 135, 19, 119, 182, 172, 36, 17, 37, 192, 108, 20, 118, 182, 155, 18, 16, 227, 135, 20, 119, 182, 172, 9, 16, 72, 18, 192, 118, 34, 247, 230, 135, 20, 34, 182, 157, 16, 72, 18, 192, 230, 135, 19, 34, 182, 172, 16, 17, 42, 192, 118, 182, 155, 1, 16, 227, 148, 1, 41, 186, 16, 227, 135, 21, 34, 178, 172, 15, 17, 44, 192, 117, 183, 158, 16, 17, 44, 192, 219, 196, 144, 115, 144, 115, 125, 241, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testMonthIndex() throws {
    let source = """
monthIndex
	"Answer the index of the month in which the receiver falls."

	| leap firstDay |
	leap _ self leap.
	12 to: 1 by: -1 do:
		[ :monthIndex |
			firstDay _ (FirstDayOfMonth at: monthIndex)
							+ (monthIndex > 2 ifTrue: [leap] ifFalse: [0]).
			firstDay<= day
				ifTrue: [^monthIndex]].
	self error: 'illegal month'

"""
    // 15 .. 54
    let expected = [112, 208, 104, 34, 118, 116, 137, 118, 200, 164, 21, 106, 67, 18, 192, 18, 119, 179, 153, 16, 144, 117, 176, 105, 17, 0, 180, 153, 18, 124, 115, 125, 131, 97, 135, 112, 37, 228, 135, 120]
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

  func testDayYear() throws {
    let source = """
day: dayInteger year: yearInteger
	day _ dayInteger.
	year _ yearInteger

"""
    // 3 .. 7
    let expected = [16, 96, 17, 97, 120]
    try runningSource(source, expecting: expected)
  }

  func testPrintOn() throws {
    let source = """
printOn: aStream
	self printOn: aStream format: #(1 2 3 32 3 1 )

"""
    // 7 .. 12
    let expected = [112, 16, 33, 240, 135, 120]
    try runningSource(source, expecting: expected)
  }

  func testLessThan() throws {
    let source = """
< aDate
	"Answer whether aDate precedes the date of the receiver."

	year = aDate year
		ifTrue: [^day < aDate day]
		ifFalse: [^year < aDate year]

"""
    // 7 .. 21
    let expected = [1, 16, 208, 182, 156, 0, 16, 209, 178, 124, 1, 16, 208, 178, 124]
    try runningSource(source, expecting: expected)
  }

  func testHash() throws {
    let source = """
hash
	"Hash is reimplemented because = is implemented."

	^(year hash bitShift: 3) bitXor: day

"""
    // 9 .. 15
    let expected = [1, 209, 34, 188, 0, 224, 124]
    try runningSource(source, expecting: expected)
  }

}
