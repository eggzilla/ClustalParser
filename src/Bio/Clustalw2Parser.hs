-- | Parse Clustalw2 output

module Bio.Clustalw2Parser (
                       parseClustalw2Alignment,
                       readClustalw2Alignment,
                       module Bio.Clustalw2Data
                      ) where

import Bio.Clustalw2Data
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language (emptyDef)    
import Control.Monad
import Data.List

readDouble :: String -> Double
readDouble = read              

readInt :: String -> Int
readInt = read

-- | Parse the input as Clustalw2Alignment datatype
genParserClustalw2Summary :: GenParser Char st Clustalw2Summary
genParserClustalw2Summary = do
  newline
  newline
  newline
  space
  string "Clustal "
  version <- many1 (noneOf " ")
  many1 (noneOf "\n")
  newline
  newline
  newline
  string "Sequence format is "
  sequenceFormat <- many1 (noneOf "\n")
  newline
  sequenceParametersList <- many1 genParserSequenceParameters
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
  return $ Clustalw2Summary version sequenceFormat sequenceParametersList pairwiseAlignmentSummaryList guideTreeFileName (readInt numberOfGroups) groupSummaryList (readInt alignmentScore) alignmentFileName

genParserGroupSummary :: GenParser Char st GroupSummary
genParserGroupSummary = do
  string "Group " 
  groupIndex <- many1 digit
  string ": Sequences:"
  many1 spaces
  sequenceNumber <- many1 digit
  many1 spaces
  string "Score:"
  groupScore <- many1 digit
  newline
  return $ GroupSummary (readInt groupIndex) (readInt sequenceNumber) (readInt groupScore)

genParserPairwiseAlignmentSummary :: GenParser Char st PairwiseAlignmentSummary
genParserPairwiseAlignmentSummary = do
  string "Sequences (" 
  firstSeqIndex <- many1 digit
  string ":"
  secondSeqIndex <- many1 digit
  string ") Aligned. Score: "
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

-- | Parse the input as Clustalw2Alignment datatype
genParserClustalw2Alignment :: GenParser Char st Clustalw2Alignment
genParserClustalw2Alignment = do
  many1 (noneOf "\n")
  newline
  newline
  newline
  alignmentSlices <- many1 genParserClustalw2AlignmentSlice
  eof  
  return (mergealignmentSlices alignmentSlices)

mergealignmentSlices :: [Clustalw2AlignmentSlice] -> Clustalw2Alignment
mergealignmentSlices slices = alignment
  where entrySlicesList = map entrySlices slices -- list of lists of entry slices
        sequenceIdentifiers = map entrySequenceSliceIdentifier (head entrySlicesList)
        alignmentEntriesListBySlice =  map (map entryAlignedSliceSequence) entrySlicesList  
        transposedAlignmentEntriesListbySlice = transpose alignmentEntriesListBySlice
        mergedAlignmentSequenceEntries = map concat transposedAlignmentEntriesListbySlice
        mergedAlignmentEntries = map constructAlignmentEntries (zip sequenceIdentifiers mergedAlignmentSequenceEntries)
        conservationTrackSlices = map conservationTrackSlice slices
        mergedConservationTrack = concat conservationTrackSlices
        alignment = (Clustalw2Alignment mergedAlignmentEntries mergedConservationTrack)

constructAlignmentEntries :: (String, String) -> Clustalw2AlignmentEntry
constructAlignmentEntries (entryIdentifier,entrySequence) = Clustalw2AlignmentEntry entryIdentifier entrySequence

-- |        
genParserClustalw2AlignmentSlice :: GenParser Char st Clustalw2AlignmentSlice
genParserClustalw2AlignmentSlice = do
  entrySlices <- many1 genParserClustalw2EntrySlice
  --extract length of identifier and spacer to determine offset of conservation track
  let offsetLenght = (length (entrySequenceSliceIdentifier (head entrySlices))) + (spacerLength (head entrySlices))
  spacerAndConservationTrackSlice <- many1 (noneOf "\n")
  let conservationTrackSlice = drop offsetLenght spacerAndConservationTrackSlice
  newline
  optional newline
  return $ Clustalw2AlignmentSlice entrySlices conservationTrackSlice

genParserClustalw2EntrySlice :: GenParser Char st Clustalw2AlignmentEntrySlice
genParserClustalw2EntrySlice = do
  sliceIdentifier <- many1 (noneOf " ")
  spacer <- many1 (char ' ')
  sliceSequence <- many1 (noneOf "\n")
  newline
  return $ Clustalw2AlignmentEntrySlice sliceIdentifier sliceSequence (length spacer)

-- | 
parseClustalw2Alignment input = parse genParserClustalw2Alignment "genParserClustalw2Alignment" input

-- |                      
readClustalw2Alignment :: String -> IO (Either ParseError Clustalw2Alignment)                  
readClustalw2Alignment filePath = parseFromFile genParserClustalw2Alignment filePath
