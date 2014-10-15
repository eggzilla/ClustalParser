-- | Parse Clustal output

module Bio.ClustalParser (
                       parseClustalAlignment,
                       readClustalAlignment,
                       parseClustalSummary,
                       readClustalSummary,
                       module Bio.ClustalData
                      ) where

import Bio.ClustalData
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language (emptyDef)    
import Control.Monad
import Data.List

readDouble :: String -> Double
readDouble = read              

readInt :: String -> Int
readInt = read

-- | Parse the input as ClustalAlignment datatype
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
  sequenceFormat <- many1 (noneOf "\n")
  newline
  sequenceParametersList <- many1 (try genParserSequenceParameters)
  string "Start of Pairwise alignments" 
  newline
  string "Aligning..."
  newline
  newline
  pairwiseAlignmentSummaryList <- many1 genParserPairwiseAlignmentSummary
  string "Guide tree file created:   ["
  guideTreeFileName <- many1 (noneOf "]")
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
  alignmentScore <- many1 digit
  newline
  newline
  string "CLUSTAL-Alignment file created  ["
  alignmentFileName <- many1 (noneOf "]")
  char ']'
  newline
  newline
  eof  
  return $ ClustalSummary version sequenceFormat sequenceParametersList pairwiseAlignmentSummaryList guideTreeFileName (readInt numberOfGroups) groupSummaryList (readInt alignmentScore) alignmentFileName

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
  groupScore <- optionMaybe (many1 digit) 
  newline
  return $ GroupSummary (readInt groupIndex) (liftM readInt sequenceNumber) (liftM readInt groupScore)

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
  newline
  newline
  newline
  alignmentSlices <- many1 genParserClustalAlignmentSlice
  eof  
  return (mergealignmentSlices alignmentSlices)

mergealignmentSlices :: [ClustalAlignmentSlice] -> ClustalAlignment
mergealignmentSlices slices = alignment
  where entrySlicesList = map entrySlices slices -- list of lists of entry slices
        sequenceIdentifiers = map entrySequenceSliceIdentifier (head entrySlicesList)
        alignmentEntriesListBySlice =  map (map entryAlignedSliceSequence) entrySlicesList  
        transposedAlignmentEntriesListbySlice = transpose alignmentEntriesListBySlice
        mergedAlignmentSequenceEntries = map concat transposedAlignmentEntriesListbySlice
        mergedAlignmentEntries = map constructAlignmentEntries (zip sequenceIdentifiers mergedAlignmentSequenceEntries)
        conservationTrackSlices = map conservationTrackSlice slices
        mergedConservationTrack = concat conservationTrackSlices
        alignment = ClustalAlignment mergedAlignmentEntries mergedConservationTrack

constructAlignmentEntries :: (String, String) -> ClustalAlignmentEntry
constructAlignmentEntries (entryIdentifier,entrySequence) = ClustalAlignmentEntry entryIdentifier entrySequence

-- |        
genParserClustalAlignmentSlice :: GenParser Char st ClustalAlignmentSlice
genParserClustalAlignmentSlice = do
  entrySlices <- many1 genParserClustalEntrySlice
  --extract length of identifier and spacer to determine offset of conservation track
  let offsetLenght = length (entrySequenceSliceIdentifier (head entrySlices)) + spacerLength (head entrySlices)
  spacerAndConservationTrackSlice <- many1 (noneOf "\n")
  let conservationTrackSlice = drop offsetLenght spacerAndConservationTrackSlice
  newline
  optional newline
  return $ ClustalAlignmentSlice entrySlices conservationTrackSlice

genParserClustalEntrySlice :: GenParser Char st ClustalAlignmentEntrySlice
genParserClustalEntrySlice = do
  sliceIdentifier <- many1 (noneOf " ")
  spacer <- many1 (char ' ')
  sliceSequence <- many1 (noneOf "\n")
  newline
  return $ ClustalAlignmentEntrySlice sliceIdentifier sliceSequence (length spacer)

-- |
parseClustalAlignment :: String -> Either ParseError ClustalAlignment 
parseClustalAlignment = parse genParserClustalAlignment "genParserClustalAlignment"

-- |                      
readClustalAlignment :: String -> IO (Either ParseError ClustalAlignment)   
readClustalAlignment = parseFromFile genParserClustalAlignment

-- | 
parseClustalSummary :: String -> Either ParseError ClustalSummary
parseClustalSummary = parse genParserClustalSummary "genParserClustalSummary"

-- | Parse Clustal format from file
readClustalSummary :: String -> IO (Either ParseError ClustalSummary)       
readClustalSummary = parseFromFile genParserClustalSummary
