#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"$Id: catfile.st,v 1.11 2012-03-02 15:10:13-08 - - $"
execname := '$RCSfile: catfile.st,v $' =~ '.*: (.*),v.*' at: 1.

nl := Character nl.

Character extend [
   isPrint [
      ^ (Character space <= self) & (self <= $~)
   ]
   visible [
      self isPrint ifTrue: [^ '$', self asString]
                   ifFalse: [^ self asInteger printStringRadix: 8]
   ]
]

Object subclass: ZeroArray [
   |theArray|

   ZeroArray class >> new [
      self shouldNotImplement.
   ]

   ZeroArray class >> new: size [
      |result|
      result := super new.
      result init: size.
      ^ result
   ]

   init: size [
      theArray := Array new: size.
   ]

   size [
      ^ theArray size.
   ]

   at: index [
      ^ theArray at: index + 1.
   ]

   at: index put: value [
      ^ theArray at: index + 1 put: value.
   ]

   incr: index [
      (self at: index) isNil
         ifTrue: [ self at: index put: 0].
      self at: index put: (self at: index) + 1.
   ]

   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]

]

Object subclass: Leaf [
   |char count|
   char [ ^ char ]
   count [ ^ count ]

   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
   ]

   <= other [
      ^ (count < other count)
      | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]

]


Leaf subclass: Tree [
   |left right|

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
   ]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
		(visitor notNil) ifTrue: [
		  left inorder: visitor prefix: string, '0'.
		  right inorder: visitor prefix: string, '1'.
		  ^ visitor.
	  ]
   ]
]

Object subclass: BitStream [
   |bitIndex byte myStream|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
   ]
   "When you start it off, (index = 1, get the next byte and set the index to 9. We are going to go from left to right"
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
   currByte [
      ^byte
   ]
]

freqtable := ZeroArray new: 256.
encodingTable := ZeroArray new: 256.
sortcol := SortedCollection new.


nl := Character nl.
Smalltalk arguments size = 1
	ifTrue: [
		Smalltalk arguments do: [:filename |
		|infile ordChar|
		infile := FileStream open: filename
							mode: FileStream read.

		[infile atEnd not] whileTrue:[
			ordChar := infile next asInteger.
			freqtable incr: ordChar.  
		].
		infile close. 
		]
	].

	


freqtable keysAndValuesDo: [:key :value |
   (value notNil and: [value > 0]) ifTrue: [
		|leaf|
      leaf := Leaf new: key asCharacter count: value.
	  sortcol add: leaf
   ]
].



freqtable keysAndValuesDo: [:key :value |
   (value notNil and: [value > 0]) ifTrue: [
	sortcol size > 1 ifTrue: [ 
		a := sortcol removeFirst.
		b := sortcol removeFirst.
		treeValue := a count + b count.
		newRoot := Tree new: a char count: treeValue left: a right: b.
		sortcol add: newRoot.
	]
    ]
].




[sortcol notEmpty] whileTrue: [
	|tree|
	tree := sortcol removeFirst.
	stdout << tree << nl << nl.
	tree inorder: [:char :string |
		|index test| 
		index := char asInteger.
		"stdout << 'char[' << char << '] encoding at index: ' << index << ' Putting string: '	<< string << nl."
		encodingTable at: index put: string.
	] prefix: ''.
	stdout<< nl
].

(0 to: encodingTable size - 1)
   do: [:index |
		((freqtable at:index) notNil) ifTrue: [
		|char encodingString freqValue |
			char := index asCharacter.
			encodingString := encodingTable at: index.
			freqValue := freqtable at: index.
			stdout << char << ' ' << freqValue << ' ' << encodingString << nl.		
	]
	]

"This is the encoding string
[sortcol notEmpty] whileTrue: [
   |first|

   first := sortcol removeFirst.
   stdout << first << nl << nl.
   first inorder: [:char :string |
   stdout << '[' << char << ']=' << ' ' << string << nl.
	] prefix: ''.
	stdout<< nl << nl
].
"






