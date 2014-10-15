-- | This module contains data structures for the Clustal tools 
--   For more information on Clustal tools consult: <http://www.clustal.org/>

module Bio.ClustalData where
    
-- | 
data ClustalSummary = ClustalSummary
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
     groupScore :: Maybe Int
  }
  deriving (Show, Eq)


-- | Data structure for 
data ClustalAlignment = ClustalAlignment
  { 
    alignmentEntries :: [ClustalAlignmentEntry],
    conservationTrack :: String
  }
  deriving (Show, Eq)

data ClustalAlignmentEntry = ClustalAlignmentEntry
  {
    entrySequenceIdentifier :: String,
    entryAlignedSequence :: String
  }
  deriving (Show, Eq)

data ClustalAlignmentSlice = ClustalAlignmentSlice
  {
    entrySlices :: [ClustalAlignmentEntrySlice],
    conservationTrackSlice :: String
  }
  deriving (Show, Eq)

data ClustalAlignmentEntrySlice = ClustalAlignmentEntrySlice
  {
    entrySequenceSliceIdentifier :: String,
    entryAlignedSliceSequence :: String,
    spacerLength :: Int
  }
  deriving (Show, Eq)

data ClustalGuideTree = ClustalGuideTree
  { 
    guideTreeEntries :: [ClustalGuideTreeEntry]
  }
  deriving (Show, Eq)

data ClustalGuideTreeEntry = ClustalGuideTreeEntry
  { 
    entryDesignation :: String,
    entryDistance :: Double
  }
  deriving (Show, Eq)

