-- | This module contains data structures for the Clustal tools 
--   For more information on Clustal tools consult: <>

module Bio.Clustalw2Data where
    
-- | 
data Clustalw2Summary = Clustalw2Summary
  {
    clustalw2version :: String,
    sequenceFormat :: String,
    sequenceAttributes :: [SequenceAttributes],
    alignmentSummaries :: [AlignmentSummary],
    guideTreeFileName :: String,
    groupNumber :: Int,
    groupSummaries :: [GroupSummary]
    alignmentScore :: Int
    alignmentFileName :: String
  }
  deriving (Show, Eq)

data Clustalw2Summary = Clustalw2Summary
  {
    clustalw2version :: String,
    sequenceFormat :: String,
    sequenceAttributes :: [SequenceAttributes],
    alignmentSummaries :: [AlignmentSummary],
    guideTreeFileName :: String,
    groupNumber :: Int,
    groupSummaries :: [GroupSummary]
    alignmentScore :: Int
    alignmentFileName :: String
  }
  deriving (Show, Eq)

-- | Data structure for 
data Clustalw2Alignment = Clustalw2Alignment
  { 
    alignmentEntries :: [Clustalw2AlignmentEntry]
    conservationTrack :: String
  }
  deriving (Show, Eq)

data Clustalw2AlignmentEntry = Clustalw2AlignmentEntry
  {
    entrySequenceIdentifier :: String
    alignedSequence :: String
  }

data Clustalw2GuideTree = Clustalw2GuideTree
  { 
    guideTreeEntries :: [Clustalw2GuideTreeEntry]
  }
  deriving (Show, Eq)

data Clustalw2GuideTreeEntry = Clustalw2GuideTreeEntry
  { 
    entryDesignation :: String
    entryDistance :: Double
  }
  deriving (Show, Eq)

