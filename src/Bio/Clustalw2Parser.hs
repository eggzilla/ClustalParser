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
