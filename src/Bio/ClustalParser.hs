-- | Parse Clustal output
--   For more information on Clustal tools consult: <http://www.clustal.org/>
module Bio.ClustalParser (
                       parseClustalAlignment,
                       readClustalAlignment,
                       parseStructuralClustalAlignment,
                       readStructuralClustalAlignment,
                       parseClustalSummary,
                       readClustalSummary,
                       module Bio.ClustalData
                      ) where

import Bio.ClustalData
import Text.ParserCombinators.Parsec    
import Control.Monad
import Data.List

readDouble :: String -> Double
readDouble = read              

readInt :: String -> Int
readInt = read

-- | Parse the input as ClustalSummary datatype
genParserClustalSummary :: GenParser Char st ClustalSummary
genParserClustalSummary = do
  newline
  newline
  newline
  space
  string "CLUSTAL "
  version <- many1 (noneOf " ")
  many1 (noneOf "\n")
  newline
  newline
  newline
  string "Sequence format is "
  sequenceFormat' <- many1 (noneOf "\n")
  newline
  sequenceParametersList <- many1 (try genParserSequenceParameters)
  string "Start of Pairwise alignments" 
  newline
  string "Aligning..."
  newline
  newline
  pairwiseAlignmentSummaryList <- many1 genParserPairwiseAlignmentSummary
  string "Guide tree file created:   ["
  guideTreeFileName' <- many1 (noneOf "]")
  char ']'
  newline
  newline
  string "There are "
  numberOfGroups <- many1 digit
  string " groups"
  newline
  string "Start of Multiple Alignment"
  newline
  newline
  string "Aligning..."
  newline
  groupSummaryList <- many1 genParserGroupSummary
  string "Alignment Score "
  alignmentScore' <- many1 digit
  newline
  newline
  string "CLUSTAL-Alignment file created  ["
  alignmentFileName' <- many1 (noneOf "]")
  char ']'
  newline
  newline
  eof  
  return $ ClustalSummary version sequenceFormat' sequenceParametersList pairwiseAlignmentSummaryList guideTreeFileName' (readInt numberOfGroups) groupSummaryList (readInt alignmentScore') alignmentFileName'

genParserGroupSummary :: GenParser Char st GroupSummary
genParserGroupSummary = do
  string "Group " 
  groupIndex <- many1 digit
  string ":"
  optional space
  optional (string "Sequences:")
  many1 space
  sequenceNumber <- optionMaybe (many1 digit)
  optional (many1 space)
  string "Score:" <|>  string "Delayed"
  groupScore' <- optionMaybe (many1 digit) 
  newline
  return $ GroupSummary (readInt groupIndex) (liftM readInt sequenceNumber) (liftM readInt groupScore')

genParserPairwiseAlignmentSummary :: GenParser Char st PairwiseAlignmentSummary
genParserPairwiseAlignmentSummary = do
  string "Sequences (" 
  firstSeqIndex <- many1 digit
  string ":"
  secondSeqIndex <- many1 digit
  string ") Aligned. Score:"
  many1 space
  pairwiseScore <- many1 digit
  newline
  return $ PairwiseAlignmentSummary (readInt firstSeqIndex) (readInt secondSeqIndex) (readInt pairwiseScore)

genParserSequenceParameters :: GenParser Char st SequenceParameters
genParserSequenceParameters = do
  string "Sequence " 
  sequenceIndexParam <- many1 digit
  string ": "
  sequenceIdentifierParam <- many1 (noneOf " ")
  spaces
  sequenceLengthParam <- many1 digit
  space
  string "bp"
  newline
  return $ SequenceParameters (readInt sequenceIndexParam) sequenceIdentifierParam (readInt sequenceLengthParam)

-- | Parse the input as ClustalAlignment datatype
genParserClustalAlignment :: GenParser Char st ClustalAlignment
genParserClustalAlignment = do
  many1 (noneOf "\n")
  many1 newline
  alignmentSlices <- many1 (try genParserClustalAlignmentSlice)
  eof  
  return (mergealignmentSlices alignmentSlices)

mergealignmentSlices :: [ClustalAlignmentSlice] -> ClustalAlignment
mergealignmentSlices slices = alignment
  where entrySlicesList = map entrySlices slices -- list of lists of entry slices
        sequenceIdentifiers = nub (map entrySequenceSliceIdentifier (head entrySlicesList))
        alignmentEntriesListBySlice =  map (map entryAlignedSliceSequence) entrySlicesList  
        transposedAlignmentEntriesListbySlice = transpose alignmentEntriesListBySlice
        mergedAlignmentSequenceEntries = map concat transposedAlignmentEntriesListbySlice
        mergedAlignmentEntries = map constructAlignmentEntries (zip sequenceIdentifiers mergedAlignmentSequenceEntries)
        conservationTrackSlices = map conservationTrackSlice slices
        mergedConservationTrack = concat conservationTrackSlices
        alignment = ClustalAlignment mergedAlignmentEntries mergedConservationTrack

constructAlignmentEntries :: (String, String) -> ClustalAlignmentEntry
constructAlignmentEntries (entryIdentifier,entrySequence) = ClustalAlignmentEntry entryIdentifier entrySequence

genParserClustalAlignmentSlice :: GenParser Char st ClustalAlignmentSlice
genParserClustalAlignmentSlice = do
  entrySlices' <- many1 genParserClustalEntrySlice
  --extract length of identifier and spacer to determine offset of conservation track
  let offsetLenght = length (entrySequenceSliceIdentifier (head entrySlices')) + spacerLength (head entrySlices')
  spacerAndConservationTrackSlice <- many1 (noneOf "\n")
  let conservationTrackSlice' = drop offsetLenght spacerAndConservationTrackSlice
  newline
  optional newline
  return $ ClustalAlignmentSlice entrySlices' conservationTrackSlice'

genParserClustalEntrySlice :: GenParser Char st ClustalAlignmentEntrySlice
genParserClustalEntrySlice = do
  sliceIdentifier <- many1 (noneOf " ")
  spacer <- many1 (char ' ')
  sliceSequence <- many1 (oneOf ".SNYRUAGCT-uagct")
  newline
  return $ ClustalAlignmentEntrySlice sliceIdentifier sliceSequence (length spacer)

--Structural Clustal Parser functions

-- | Parse the input as ClustalAlignment datatype as used in mlocarna
genParserStructuralClustalAlignment :: GenParser Char st StructuralClustalAlignment
genParserStructuralClustalAlignment = do
  genParseMlocarnaHeader
  alignmentSlices <- many1 (try genParserStructuralClustalAlignmentSlice)
  secondaryStructure <- genSecondaryStructure  
  energy' <- genParseEnergy
  eof  
  return (mergeStructuralAlignmentSlices alignmentSlices secondaryStructure energy')

genSecondaryStructure :: GenParser Char st String
genSecondaryStructure = do
  string "alifold"
  many1 space
  secondaryStructure <- many1 (oneOf ".()")
  space
  return secondaryStructure

genParseEnergy :: GenParser Char st Double
genParseEnergy = do
  string "("
  many space 
  energy' <- many1 (noneOf " ")
  optional space
  char ('=')
  many1 (noneOf "\n")
  newline  
  return (readDouble energy')

genParseMlocarnaHeader :: GenParser Char st String
genParseMlocarnaHeader = do
  string "mLocARNA"
  many1 (choice [alphaNum,oneOf "-: ()."])
  newline
  string "Copyright"
  many1 (choice [alphaNum,char ' '])
  newline
  newline  
  string "Compute pair probs ..."
  newline
  string "Perform progressive alignment ..."
  many1 newline
  return ""

mergeStructuralAlignmentSlices :: [StructuralClustalAlignmentSlice] -> String -> Double -> StructuralClustalAlignment
mergeStructuralAlignmentSlices slices secondaryStructure energy' = alignment
  where entrySlicesList = map structuralEntrySlices slices -- list of lists of entry slices
        sequenceIdentifiers = (map structuralEntrySequenceSliceIdentifier (head entrySlicesList))
        alignmentEntriesListBySlice =  map (map structuralEntryAlignedSliceSequence) entrySlicesList  
        transposedAlignmentEntriesListbySlice = transpose alignmentEntriesListBySlice
        mergedAlignmentSequenceEntries = map concat transposedAlignmentEntriesListbySlice
        mergedAlignmentEntries = map constructStructuralAlignmentEntries (zip sequenceIdentifiers mergedAlignmentSequenceEntries)
        alignment = StructuralClustalAlignment mergedAlignmentEntries secondaryStructure energy' 

constructStructuralAlignmentEntries :: (String, String) -> ClustalAlignmentEntry
constructStructuralAlignmentEntries (entryIdentifier,entrySequence) = ClustalAlignmentEntry entryIdentifier entrySequence

genParserStructuralClustalAlignmentSlice :: GenParser Char st StructuralClustalAlignmentSlice
genParserStructuralClustalAlignmentSlice = do
  entrySlices' <- many1 (try genParserStructuralClustalEntrySlice)
  optional newline
  return $ StructuralClustalAlignmentSlice entrySlices'

genParserStructuralClustalEntrySlice :: GenParser Char st StructuralClustalAlignmentEntrySlice
genParserStructuralClustalEntrySlice = do
  sliceIdentifier <- many1 (noneOf " ")
  many1 (char ' ')
  sliceSequence <- many1 (oneOf ".SNYRUAGCT-uagtc")
  newline
  return $ StructuralClustalAlignmentEntrySlice (filter (/='\n') sliceIdentifier) sliceSequence

-- exported functions

-- | Parse Clustal alignment (.aln) from String
parseClustalAlignment :: String -> Either ParseError ClustalAlignment 
parseClustalAlignment = parse genParserClustalAlignment "genParserClustalAlignment"

-- | Parse Clustal alignment (.aln) from filehandle                  
readClustalAlignment :: String -> IO (Either ParseError ClustalAlignment)   
readClustalAlignment = parseFromFile genParserClustalAlignment

-- | Parse Clustal alignment (.aln) with secondary structure in dot-bracket notation from String (as produced by mlocarna)
parseStructuralClustalAlignment :: String -> Either ParseError StructuralClustalAlignment 
parseStructuralClustalAlignment = parse genParserStructuralClustalAlignment "genParserStructuralClustalAlignment"

-- | Parse Clustal alignment (.aln) with secondary structure in dot-bracket notation from filehandle (as produced by mlocarna)                  
readStructuralClustalAlignment :: String -> IO (Either ParseError StructuralClustalAlignment)   
readStructuralClustalAlignment = parseFromFile genParserStructuralClustalAlignment

-- |  Parse Clustal summary (printed to STDOUT) from String
parseClustalSummary :: String -> Either ParseError ClustalSummary
parseClustalSummary = parse genParserClustalSummary "genParserClustalSummary"

-- | Parse Clustal summary (printed to STDOUT) from file
readClustalSummary :: String -> IO (Either ParseError ClustalSummary)       
readClustalSummary = parseFromFile genParserClustalSummary
