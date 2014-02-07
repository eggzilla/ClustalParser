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
  alignmentSections <- many1 genParserClustalw2AlignmentAlignmentSections
  eof  
  return $ Clustalw2Alignment alignmentSections

-- |        
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
