-- | Parse Clustalw2 output

module Bio.Clustalw2Parser (
                       parseClustalw2,
                       readClustalw2,
                       module Bio.Clustalw2Data
                      ) where

import Bio.Clustalw2Data
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language (emptyDef)    
import Control.Monad

readDouble :: String -> Double
readDouble = read              

readInt :: String -> Int
readInt = read

-- | Parse the input as Clustalw2Alignment datatype
genParserClustalw2Alignment :: GenParser Char Clustalw2Alignment
genParserClustalw2Alignment = do
  many1 (noneOf "\n")
  many1 newline
  alignmentSlices <- many1 genParserClustalw2AlignmentSlice
  eof  
  return (mergealignmentSlices alignmentSlices)

mergealignmentSlices :: [Clustalw2AlignmentSlice] -> Clustalw2Alignment
mergealignmentSlices slices = alignment
  where entrySlicesList = map entrySlices slices -- list of lists of entry slices
        sequenceIdentifiers = map entrySequenceSliceIdentifier (head entrySlicesList)
        alignmentEntriesListbySlice = map entryAlignedSequence slices
        transposedalignmentEntriesListbySlice = transpose alignmentEntriesListbySlice
        mergedalignmentSeqEntries = map concat transposedalignmentEntriesListbySlice
        mergedAlignmentEntries = map constructAlignmentEntries (zip sequenceIdentifiers mergedalignmentSeqEntries)
        conservationTrackSlices = map conservationTrackSlice slices
        mergedConservationTrack = concat conservationTrack
        alignment = Clustalw2Alignment mergedAlignmentEntries mergedConservationTrack

constructAlignmentEntries :: (String, String) -> Clustalw2AlignmentEntry
constructAlignmentEntries (entryIdentifier,entrySequence) = Clustalw2AlignmentEntry entryIdentifier entrySequence

-- |        
genParserClustalw2AlignmentSlice :: GenParser Char st Clustalw2AlignmentSlice
genParserClustalw2AlignmentSlice = do
  entrySlices <- many1 genParserClustalw2EntrySlice
  many1 space
  conservationTrackSlice <- many1 (noneOf "\n")
  newline
  optional newline
  return $ Clustalw2AlignmentSlice entrySlices conservationTrackSlice

genParserClustalw2EntrySlice :: GenParser Char st Clustalw2AlignmentEntrySlice
genParserClustalw2EntrySlice = do
  sliceIdentifier <- many1 (noneOf "\n")
  many1 space
  sliceSequence <- many1 (noneOf "\n")
  newline
  return $ Clustalw2AlignmentEntrySlice

genParserClustalw2AlignmentSections :: GenParser Char st Clustalw2AlignmentSection
genParserClustalw2AlignmentSections = do
  string (">") 
  sequenceIdentifier <- many1 (noneOf "\n")                
  newline
  sequence <- many1 (noneOf "\n")                
  newline 
  secondaryStructure <- many1 (oneOf "&().,")
  space
  string ("(")
  foldingEnergy <- many1 (noneOf ")")
  string (")")
  return $ Clustalw2AlignmentSection sequenceIdentifier sequence secondaryStructure (readDouble foldingEnergy)

-- | 
parseClustalw2Alignment input = parse genParserClustalw2Alignment "genParserClustalw2Alignment" input

-- |                      
readClustalw2Alignment :: String -> IO (Either ParseError Clustalw2Alignment)                  
readClustalw2Alignment filePath = parseFromFile genParserClustalw2Alignment filePath
