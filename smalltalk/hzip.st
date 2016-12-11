#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

endl := Character nl.


" *****************************************
  *********** Class Declaration ***********
  ***************************************** "


"Provided by Mackey. Modified to fit the assginment output"
Character extend [
   "Check if the character is printable."
   isPrint [ ^ (Character space <= self) & (self <= $~) ]
   "Pad spaces to fit project description"
   visible [ 
      self isPrint ifTrue:  [^ '  ', self asString]
                   ifFalse: [
                     |in out counter|
                     counter := 3.
                     in  := self asInteger asString.
                     out := '   '.
                     (in size) to: 1 by: -1 do:[ :index|
                        out at:counter put: (in at: index).
                        counter := counter - 1.
                     ].
                     ^out.
                   ]
   ]
]

"Modified to fit the assginment output"
Integer extend[
   "Pad spaces to fit project description"
   visible[
      (self asString size < 5) ifTrue:[
         |in out counter|
         counter := 5.
         in := self asString.
         out := '     '.
         (in size) to: 1 by: -1 do:[ :index|
            out at:counter put: (in at: index).
            counter := counter - 1.
         ].
         [ counter > 0 ] whileTrue:[
            out at:counter put: Character space.
            counter := counter - 1.

         ].
         ^out.
      ].
   ]
]

"Modified to get frequency table"
FileStream extend [
   getFrequencyTable [
      |fTable|
      fTable := ZeroArray new: 257.
      [self atEnd not] whileTrue: [
         |ordChar|
         ordChar := self next asInteger.
         fTable incr: ordChar.
      ].
      fTable incr: 256.
      ^ fTable
   ]
]

"A super class to call inspect when debug option is on"
Object subclass: BasicDebug[
   debug: option[ option debugOn ifTrue:[self inspect. '' displayNl]. ]
]

"Command line option scanner."
Object subclass: Option[
   |debug table comps decom input output status exitMessage|

   "Getters"
   debugOn [^ debug]  tableOn     [^ table] 
   compsOn [^ comps]  decomOn     [^ decom]
   input   [^ input]  output      [^ output]   
   status  [^ status] exitMessage [^ exitMessage ]

   Option class >> new [ |out| out := super new. out init. ^out. ]

   "Scans the command line option and set option flags"
   init[
      "Default values"
      debug := false. table := false. 
      comps := false. decom := false.
      input := ''.    output := stdout.
      status := 0.    exitMessage := ''.

      "Scan options and input arguments"
      Smalltalk arguments: '-t -c -d -u'
      do: [:opt :arg |
         (opt == $t) ifTrue: [table := true].
         (opt == $c) ifTrue: [comps := true].
         (opt == $d) ifTrue: [debug := true].
         (opt == $u) ifTrue: [decom := true].
         (arg class = String) ifTrue: [
            (input isEmpty)
               ifTrue:  [ input  := arg. ]
               ifFalse: [ output := arg. ].
         ].
      ] ifError: [
         self setStatusExit: 1 message: 'Error: invalid option'.
      ].
   ]

   "Program can not excuate more than one option."
   checkOptions[
      ((comps & decom) | (comps & table) | (table & decom)) ifTrue:[
         self setStatusExit: 1 message: 
         'Error: Can not operate more than one option at same time'.
      ].
      ((comps = false) & (table = false) & (decom = false)) ifTrue:[
         self setStatusExit: 0 message: 
         'Notice: No excuation is selected'.
      ].
   ]

   "Check if user has enter a input file in the command line"
   checkInputFile[ 
      (input = '') ifTrue:[ 
         self setStatusExit: 1 message: 'No input file.'
      ].
   ]

   "This method should only be call then the program is quitting"
   setStatusExit: toSet message: toShow[ 
      status := toSet. 
      exitMessage := toShow.
      self exit. 
      ObjectMemory quit: status.
   ]

   "Prints exit message and status when debug option is on"
   exit[
      (self debugOn) ifTrue: [
         self inspect.
         '' displayNl.
         exitMessage displayNl.
         'Exit status: ' display.
         status displayNl.
      ].
   ]   
]

"Provided by Mackey. Use this for frequency table"
Object subclass: ZeroArray [
   |theArray|
   ZeroArray class >> new [ self shouldNotImplement. ]
   ZeroArray class >> new: size [
      |result| result := super new. result init: size. ^ result
   ]

   init: size [ theArray := Array new: size. ]
   size [^ theArray size.]
   at: index [^ theArray at: index + 1.]
   at: index put: value [^ theArray at: index + 1 put: value.]

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

   "If the frequency is valid, get an leaf instance at the given index"
   getLeaf: index[
      |leaf|
      ((self at: index) == NULL) ifFalse: [
         leaf := Leaf new: (index asCharacter)  count: (self at: index).
      ].
      ^leaf.
   ]

   encodeToFrequencyTable: frequencyTable fileManager: file[
      (0 to: self size - 1)
         do: [:index |
         |count bitString|
         count := frequencyTable at: index.
         bitString := self at: index.
         count isInteger ifTrue: [
            file writeString: index asCharacter visible.
            file writeString: Character space.
            file writeString: count visible.
            file writeString: Character space.
            file writeString: bitString.
            file writeString: Character nl.
         ].
      ]. 
   ]

   "Needs special debug output"
   debug: option[
      (option debugOn) ifTrue:[
         (0 to: self size - 1)
         do: [:index |
            | item |
            item := self at: index.
            (item = NULL) ifFalse:[
               ((index asCharacter) visible) display.
               ' ' display. item displayNl.
            ].
         ].
      ].
   ]
]

"Manages input and output files"
BasicDebug subclass: FileManager[
   | inputFile outputFile |
   
   "Basic file"
   inputFile [^ inputFile  ]  resetInput [ inputFile  reset ]
   outputFile[^ outputFile ]  closeInput [ inputFile  close ]

   FileManager class >> new [ self shouldNotImplement. ]
   FileManager class >> new: option[
      | out | out := super new. out init: option. ^out.
   ]

   "Use stdout for output unless user specified a output file"
   init: option[
      inputFile  := FileStream open: (option input)
                               mode: FileStream read.
      ((option output) = stdout) 
      ifTrue: [ outputFile := stdout ]
      ifFalse:[
         outputFile := FileStream open: (option output) 
                                  mode: FileStream write.
      ].
   ]

   "Check if user enter an empty file"
   checkInputEmpty: option[
      (inputFile size = 0) ifTrue:[
         option setStatusExit: 0 message:
         'Notice: User did not enter a file any any data.'. 
      ].
   ]

   "Write a single char to the output file"
   writeChar: char[
      outputFile nextPutByte: char asInteger.
   ]

   "Write a string to the output file"
   writeString: string[
      outputFile << string asString.
   ]
]

"Provided by Mackey. Modified to write bits to a bitStream"
BasicDebug subclass: Leaf [
   |char count|

   "Getters"
   char [ ^ char ] count [ ^ count ] isLeaf [^ true]
   isEOF [ ^(self char asInteger = 256) ]

   Leaf class >> new [ self shouldNotImplement ]
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

   "Smalltalk uses this method to sort"
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

   "Sets the bitStream with the byte this leaf instance is holding"
   bitOut: bitStream [
      bitStream autoBit: 0.
      (1 to: 8) do: [:index | 
          bitStream autoBit: ( (self char asInteger) bitAt: index) 
      ].
      "As assginment requirement"
      char asInteger = 0   ifTrue: [ bitStream autoBit: 0. ].
      char asInteger = 256 ifTrue: [ bitStream autoBit: 1. ]
   ]
]

"Provided by Mackey. Modified to write bits to a bitStream"
Leaf subclass: Tree [
   |left right|

   "Getters"
   left[^ left] right[^ right] isLeaf [ ^ false ] isEOF[ ^false ]

   Tree class >> new: aChar count: aCount [ self shouldNotImplement ]
   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]
   setLeft: aLeft andRight: aRight [ left := aLeft. right := aRight. ]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]

   "This is not a leaf, so it has no character information."
   bitOut: bitStream [
      left  bitOut: bitStream. right bitOut: bitStream.
      "As assginment requirement"
      bitStream autoBit: 1. 
   ]
]

"Provided by Mackey."
BasicDebug subclass: BitStreamManager[
   |stringIndex bitIndex byte myStream writeIndex writeByte|
   BitStreamManager class >> new [ self shouldNotImplement. ]
   BitStreamManager class >> new: fileStream [
      |result| result := super new. result init: fileStream. ^result
   ]
   init: fileStream [
      myStream   := fileStream. bitIndex := 1.
      writeIndex := 9. writeByte := 0.
   ]
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

   autoBit: toSet [
      writeIndex := writeIndex - 1.
      writeByte := writeByte bitAt: writeIndex put: toSet.
      writeIndex = 1 ifTrue: [
         myStream nextPutByte: writeByte.
         writeByte := 0.
         writeIndex := 9.
      ].
   ]
   
   "Set 0 or 1 for each index in the string"
   autoString: toSet [
      toSet do: [:index|
         (index asString = '0')
            ifTrue:  [self autoBit: 0]
            ifFalse: [self autoBit: 1].
      ].
   ]

   "When bit stream ends not at a byte, pad zeros"
   padZeros [
      [writeIndex > 1 & writeIndex < 9] whileFalse: [ 
         self autoBit: 0. 
         "writeIndex := writeIndex - 1."
      ].
   ]
]

"This class decodes the Huffmantree from the bit stream"
BasicDebug subclass: Decoder[
   | inBitStream stack decodeTree file|
   "Getters"
   huffmanRoot[^ decodeTree]

   Decoder class >> new [ self shouldNotImplement. ]
   Decoder class >> new: fileManager[
      | out | out := super new. out init: fileManager. ^out. 
   ]
   init: fileManager [
      stack        := Stack new. 
      decodeTree   := NULL. 
      file         := fileManager.
      inBitStream  := BitStreamManager new: (fileManager inputFile).
   ]
   
   decodeKey[
      |done|
      done := false.
      [(inBitStream atEnd not) & done = false] whileTrue: [
         |decodedBit|
         decodedBit := inBitStream nextBit.
         (decodedBit = 0) 
         ifTrue: [ self readNextEightBit. ] 
         ifFalse:[ 
            decodeTree := stack leafFormTree. 
            done := stack complete.
         ].
      ].
   ]

   "Read next 8 bits, and add correspoding integer bit"
   readNextEightBit[
      | charAsInt base|
      charAsInt := 0. base :=1.
      (1 to:8) do: [:index |
            |cursor|
            cursor := inBitStream nextBit.
            (cursor = 1) ifTrue: [ charAsInt := charAsInt + base. ].
         base := base *2.
      ].
      "Spcial case: When 0 00000000, read one bit to check 0 or EOF"
      (charAsInt = 0) 
      ifFalse: [ stack push: (Leaf new: charAsInt asCharacter count:1).]
      ifTrue: [
         (inBitStream nextBit = 1) 
         ifTrue: [stack push: (Leaf new: (256 asCharacter) count:1).]
         ifFalse:[stack push: (Leaf new: (0 asCharacter) count:1).].
      ].
   ]
   
   decodeValue[
      | target |
      target := self huffmanRoot.
      [(inBitStream atEnd not) & (target isEOF = false)] whileTrue: [
         (inBitStream nextBit = 0) 
         ifTrue:  [ target := target left.  ]
         ifFalse: [ target := target right. ].

         target isLeaf ifTrue: [
            file writeChar: target char.
            target := self huffmanRoot.
         ].
      ].
   ]

]

"A Min Prority Queue implemented using a SortedCollection"
BasicDebug subclass: MinQueue[
   | queue |
   MinQueue class >> new[ | out | out := super new. out init. ^out. ]
   init[ queue := SortedCollection new. ]

   "Insert only if the tree is not NULL"
   insert: tree[ (tree == NULL) ifFalse:[ queue add: tree ]. ]
   size [^ queue size.]
   extract[ ^ queue removeFirst. ]

   "Build Huffmantree. Extract two and combine into a tree. Insert back"
   leafFormTree[
      |a b|
      [self size > 1] whileTrue:[
         a := (self extract). b := (self extract).         
         self insert: (Tree  new: (a char) count: (a count + b count) 
                        left: a right: b).
      ]
   ]
]

"A Stack implemented using a OrderedCollection"
BasicDebug subclass: Stack[
   | stack |
   Stack class >> new[ | out | out := super new. out init. ^out. ]
   init [ stack := OrderedCollection new. ]

   "Stack operations"
   push: toPush[ stack addFirst: toPush ]
   pop   [^ stack removeFirst ]
   size  [^ stack size]
   empty [^(stack size = 0)]

   "Decode Huffmantree back when encounter bit 1."
   leafFormTree[
      |right left|
      (self size > 0) 
      ifTrue: [
         right := self pop.
         (self empty)
         "The only tree left, Huffmantree is built" 
         ifTrue: [ ^right. ] 
         "Not empty, keep building Huffmantree"
         ifFalse: [
            left := self pop.
            self push: (Tree new: (left char) count: 1 
                             left: left right: right).
            ^ NULL.
         ].
      ].
   ]
   complete[ ^self empty ]

]


" *****************************************
  ************ Main Execution *************
  ***************************************** "


"Create option object and scan command line arguments"
option := Option new.
option checkInputFile.
option checkOptions.

"Use Option object's input/output string to open files"
fileManager := FileManager new: option.
fileManager checkInputEmpty: option.
fileManager debug: option.

"-u User selects the decompress option, perform decompression then exit"
(option decomOn) ifTrue:[
   decoder := Decoder new: fileManager.
   decoder decodeKey.
   decoder decodeValue.
   option setStatusExit: 0 message: 'SUCCESS'.
].


"Create a frequency table using FileManager object's inputFile"
fTable := (fileManager inputFile) getFrequencyTable.
fTable debug: option.


"Create a MinQueue object and insert nodes from the frequency table"
myQueue := MinQueue new.
(0 to: fTable size -1) do: [ :index |
   myQueue insert: (fTable getLeaf: index).
].
myQueue debug: option.


"Create Huffman encoding tree from the MinQueue object"
myQueue leafFormTree.
huffmanTree := myQueue extract.
huffmanTree debug: option.


"Create a encode table from Huffman tree's path to leaf"
encodeTable := ZeroArray new: 257.
huffmanTree inorder: [:char :bitString |
    |index count|
    index := char asInteger.
    encodeTable at: index put: bitString.
] prefix: ''.


"-t User selects the table option, print table and bitString then exit"
(option tableOn) ifTrue:[
   encodeTable encodeToFrequencyTable: fTable fileManager: fileManager.
   option setStatusExit: 0 message: 'SUCCESS'.
].

"-c User selects the compress option, perform compression then exit"
(option compsOn) ifTrue:[
   | decodeKey |
   bitStream := BitStreamManager new: (fileManager outputFile).
   bitStream debug: option.
   fileManager resetInput.
   huffmanTree bitOut: bitStream.
   bitStream autoBit: 1.
   [(fileManager inputFile) atEnd not] whileTrue: [
      |index|
      index := (fileManager inputFile) next asInteger.
      decodeKey := (encodeTable at: index).
      bitStream autoString: decodeKey.
   ].
   decodeKey := encodeTable at: 256.
   bitStream autoString: decodeKey.
   bitStream padZeros.
   fileManager closeInput.
   option setStatusExit: 0 message: 'SUCCESS'.
].






