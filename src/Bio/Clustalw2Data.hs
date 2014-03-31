-- | This module contains data structures for the Clustal tools 
--   For more information on Clustal tools consult: <>

module Bio.Clustalw2Data where
    
-- | 
data Clustalw2Summary = Clustalw2Summary
  {
    clustalw2version :: String,
    sequenceFormat :: String,
    parametersOfInputSequences :: [SequenceParameters],
    pairwiseAlignmentSummaries :: [PairwiseAlignmentSummary],
    guideTreeFileName :: String,
    groupNumber :: Int,
    groupSummaries :: [GroupSummary],
    alignmentScore :: Int,
    alignmentFileName :: String
  }
  deriving (Show, Eq)

data SequenceParameters = SequenceParameters
  {
     inputSequenceIndex :: Int,
     inputSequenceIdentifier :: String,
     inputSequenceLength :: Int
  }
  deriving (Show, Eq)

data PairwiseAlignmentSummary = PairwiseAlignmentSummary
  {
     firstSequenceIndex :: Int,
     secondSequenceIndex :: Int,
     pairwiseAlignmentScore :: Int
  }
  deriving (Show, Eq)

data GroupSummary = GroupSummary
  {
     alignmentGroupIndex :: Int,
     numberOfAlignedSequences :: Maybe Int,
     groupScore :: Int
  }
  deriving (Show, Eq)


-- | Data structure for 
data Clustalw2Alignment = Clustalw2Alignment
  { 
    alignmentEntries :: [Clustalw2AlignmentEntry],
    conservationTrack :: String
  }
  deriving (Show, Eq)

data Clustalw2AlignmentEntry = Clustalw2AlignmentEntry
  {
    entrySequenceIdentifier :: String,
    entryAlignedSequence :: String
  }
  deriving (Show, Eq)

data Clustalw2AlignmentSlice = Clustalw2AlignmentSlice
  {
    entrySlices :: [Clustalw2AlignmentEntrySlice],
    conservationTrackSlice :: String
  }
  deriving (Show, Eq)

data Clustalw2AlignmentEntrySlice = Clustalw2AlignmentEntrySlice
  {
    entrySequenceSliceIdentifier :: String,
    entryAlignedSliceSequence :: String,
    spacerLength :: Int
  }
  deriving (Show, Eq)

data Clustalw2GuideTree = Clustalw2GuideTree
  { 
    guideTreeEntries :: [Clustalw2GuideTreeEntry]
  }
  deriving (Show, Eq)

data Clustalw2GuideTreeEntry = Clustalw2GuideTreeEntry
  { 
    entryDesignation :: String,
    entryDistance :: Double
  }
  deriving (Show, Eq)

