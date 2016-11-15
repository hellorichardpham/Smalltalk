#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"$Id: catfile.st,v 1.11 2012-03-02 15:10:13-08 - - $"
execname := '$RCSfile: catfile.st,v $' =~ '.*: (.*),v.*' at: 1.

"The current problem is:
the EOF character is 01100. When decompressing, we expect 8 bits?

Are we writing
0(8-bit representation of character)________(encoding string)
Is there a way for us to test if our compression works correctly?

"


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
   
   
   postorderBitConverstion: stream [
		|ascii|
		stream setBit: 0.
		ascii := char asInteger.
		stdout << 'ascii: ' << ascii << ' '.
		(8 to: 1 by: -1) do: [:index|
			stream setBit: (ascii bitAt: index).
			].
			ascii = 256 ifTrue: [
				stream setBit: 1.
			].
			ascii = 0 ifTrue: [
				stream setBit: 0.
			].
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
      postorderBitConverstion: stream [
      "Left child, then right child, then print for current node."
      left postorderBitConverstion: stream.
      right postorderBitConverstion: stream.
	  "This line is for when we hit the inner nodes"
      stream setBit: 1.
   ]
]

Object subclass: BitStream [
   |bitIndex byte myStream bitIndexToSet byteResult|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
	  byteResult := 0.
      bitIndexToSet := 9.
   ]
   
    setBit: bit [
      bitIndexToSet := bitIndexToSet - 1.
      byteResult := byteResult bitAt: bitIndexToSet put: bit.
      bitIndexToSet = 1 ifTrue: [
		"When bitIndexToSet is 1, we have written all 8 bits"
         myStream nextPutByte: byteResult.
         byteResult := 0.
         bitIndexToSet := 9.
      ].
   ]
   
   "When you start it off, (index = 1, get the next byte and set the index to 9. We are going to go from left to right"
   readNextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   
   readString: inputString [
		inputString do:[:charIndex| 
			(charIndex asString = '1')
			ifTrue: [ self setBit: 1. ]
			ifFalse: [self setBit: 0.].
		]
   ]
   
   pad [
	[bitIndexToSet < 9] whileTrue: [
		self setBit:0.
	]
   ]
   
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
   currByte [
      ^byte
   ]
   currBitIndex [
		^bitIndexToSet
   ]
]

freqtable := ZeroArray new: 257.
encodingTable := ZeroArray new: 257.
sortcol := SortedCollection new.
nl := Character nl.
cFlag := false.
dFlag := false.
tFlag := false.
uFlag := false.
infile := nil.
outfile := stdout.
tree := nil.

(1 to: Smalltalk arguments size)
do: [:index |
	|args|
 args := (Smalltalk arguments at: index).
		(args includes: $-) ifTrue: [
			(args includes: $c) ifTrue: [cFlag := true.].
			(args includes: $d) ifTrue: [dFlag := true.].
			(args includes: $t) ifTrue: [tFlag := true.].
			(args includes: $u) ifTrue: [uFlag := true.].
		] ifFalse: [
			infile = nil ifTrue: [
				infile := FileStream open: args
							mode: FileStream read.
			] ifFalse: [
				"This is the case in that infile has been set"
				outfile := FileStream open: args
							mode: FileStream write.
			]
		].
 stdout << 'args: ' << args << nl.
].

[infile atEnd not] whileTrue:[
			ordChar := infile next asInteger.
			freqtable incr: ordChar.  
		].
infile reset.
"Add EOF with value of 1"
 freqtable incr: 256.


freqtable keysAndValuesDo: [:key :value |
   (value notNil and: [value > 0]) ifTrue: [
		|leaf|
      leaf := Leaf new: key asCharacter count: value.
	  sortcol add: leaf
   ]
].

"Create the tree"
freqtable keysAndValuesDo: [:key :value |
   (value notNil and: [value > 0]) ifTrue: [
	sortcol size > 1 ifTrue: [ 
		a := sortcol removeFirst.
		b := sortcol removeFirst.
		treeValue := a count + b count.
		newRoot := Tree new: a char count: treeValue left: a right: b.
		sortcol add: newRoot.
	]
    ].
].



uFlag ifFalse: [
	sortcol notEmpty ifTrue: [
		tree := sortcol removeFirst.
	].
].

"Create the encoding Table"
uFlag ifFalse: [
	"Create the encoding table"
	tree inorder: [:char :string |
		|asciiChar test| 
		asciiChar := char asInteger.	
		encodingTable at: asciiChar put: string.
	] prefix: ''.
].

"Traverse the tree and create the encoding string 
at the same time as creating the encoding table
outputStream is our output file. We will write the
encoding string through the stream"

cFlag ifTrue: [
outputStream := BitStream new: outfile.
	"postorderBitConversion converts the characters in the tree into bits
	In the leaf's function, it'll set the first bit as 0 then print 8
	more bits"
	tree postorderBitConverstion: outputStream.
	"End the stream with a 1 so 
	later we can determine when we done"	
	outputStream setBit: 1.
	
	[infile atEnd not] whileTrue:[
			|asciiChar byteResult|
			asciiChar := infile next asInteger.
			encodingString := (encodingTable at: asciiChar).
			outputStream readString: encodingString.
			stdout << 'Reading encoding String: ' << encodingString << nl.
		].
	eofString := (encodingTable at: 256).
	outputStream readString: eofString.
	stdout << '2. Current Bit Index: ' << outputStream currBitIndex << nl.

	outputStream pad.
	stdout << 'After padding: Current Bit Index: ' << outputStream currBitIndex << nl.
	infile close.
	outfile close.
].

"output the decoding tree to stdout 
regardless of whether an outfile is present
This is in the form of an encoding table"
tFlag ifTrue: [
tree inorder: [:char :string |
		|asciiChar| 
		asciiChar := char asInteger.
		encodingTable at: asciiChar put: string.
	] prefix: ''.
	
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
].

dFlag ifTrue: [
	inputFile := BitStream new: infile.
	outputStream := BitStream new: outfile.
	treeStack := OrderedCollection new.

	[inputFile atEnd] whileFalse: [
		byteResult := 0.
		bit := inputFile readNextBit.
"
If it is a 0, read the next 8 bits and reconstruct the byte
"
	stdout << 'This is the initial bit: ' << bit << nl.
	
	(bit = 0) ifTrue: [
		|nextBit|
		(8 to:1 by:-1) do: [:bitIndex| 
			nextBit := inputFile readNextBit.
			stdout << nextBit << ' '.
			byteResult := byteResult bitAt: bitIndex put: nextBit.
		].
		stdout << 'Byte Result: ' << byteResult << nl.
		(byteResult = 0) ifTrue: [
			"Read one more bit and see if it's a 1"
			nextBit := inputFile readNextBit.
			(nextBit = 1) ifTrue: [
				"This is adding 256 to the byte since the 9th index 
				in big endian form is 256" 
				byteResult := byteResult bitAt: 9 put: 1.
				"push this byteResult onto the stack"
				"Not sure if this is right or if it should be 256"
				stdout << 'ByteResult: ' << byteResult << nl.
				treeStack add: (Leaf new: byteResult count: 1).
			]
		] ifFalse: [
			"The byte result is non-0"
			stdout << 'ByteResult: ' << byteResult << nl.
			treeStack add: (Leaf new: byteResult count: 1).
		]
	] ifFalse: [
		"If it is a 1, pop the 1-subtree from the stack, then pop the 0-subtree from the
		stack, and create a new tree with both of those children and push the new tree
		back on the stack.
		"
		(treeStack size > 1) ifTrue: [
			rightSubtree = treeStack removeLast.
			leftSubtree = treeStack removeLast.
			"newRoot := Tree new: a char count: treeValue left: a right: b."
			newTree := Tree new: '-1' count: -1 left: leftSubtree right: rightSubtree.
		]
	]
	]
].