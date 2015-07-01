module ClustalParserSpec (spec) where

import Bio.ClustalParser 
import Test.Hspec
import Test.Hspec.QuickCheck
import Text.Parsec.Error
import Text.Parsec.Pos
import Text.ParserCombinators.Parsec.Error

spec :: Spec
spec = do
  describe "Clustal W Parser" $ do
    context "Parsing RNAalifold input" $ do
      it "Returns ClustalAlignment type" $ do
        (parseClustalAlignment clustalExample) `shouldBe` (Right clustalResult)
    context "Parsing invalid input" $ do
      it "Returns ParseError" $ do
        (parseClustalAlignment "invalid input") `shouldBe` Left clustalParseError
  describe "Structural Clustal Parser" $ do
    context "Parsing Structural Clustal input" $ do
      it "Returns StructuralClustalAlignment type" $ do
        (parseStructuralClustalAlignment structuralClustalExample) `shouldBe` Right structuralClustalResult
    context "Parsing invalid input" $ do
      it "Returns ParseError" $ do
        (parseStructuralClustalAlignment "invalid input") `shouldBe` Left structuralClustalParseError
 
clustalParseError :: ParseError
clustalParseError = addErrorMessage (Expect "lf new-line") (addErrorMessage (SysUnExpect "") (newErrorMessage clustalErrorMessage clustalErrorSourcePosition))

clustalErrorMessage :: Message
clustalErrorMessage = SysUnExpect ""

clustalErrorSourcePosition :: SourcePos
clustalErrorSourcePosition = newPos "genParserClustalAlignment" 1 14

clustalExample :: String
clustalExample = "CLUSTAL 2.1 multiple sequence alignment\n\n\nNC_0123:123-540 AATATATATTTTTTTTTTTTTTTTTTTTTTAAAAAAAAAAAAAAAAAAAAAAATTTTTTT\nNC_0223:123-540 AATATATATTTTTTTTTTTTTTTTTTTTTTAAAAAAAAAAAAAAAAAAAAAAATTTTTTT\nNC_0323:123-540 AATATATATTTTTTTTTTTTTTTTTTTTTTAAAAAAAAAAAAAAAAAAAAAAATTTTTTT\n                ************************************************************\nNC_0123:123-540 TTTTTTTTTTTTTT----------------------------------------------\nNC_0223:123-540 TTTTTTTTTTTTTTAATATATATTTTTTTTTTTTTTTTTTTTTTAAAAAAAAAAAAAAAA\nNC_0323:123-540 TTTTTTTTTTTTTTAATATATATTTTTTTTTTTTTTTTTTTTTTAAAAAAAAAAAAAAAA\n                **************                                              \n\nNC_0123:123-540 ----------------------------\nNC_0223:123-540 AAAAAAATTTTTTTTTTTTTTTTTTTTT\nNC_0323:123-540 AAAAAAATTTTTTTTTTTTTTTTTTTTT\n                *                           \n"

clustalResult :: ClustalAlignment
clustalResult = ClustalAlignment [ClustalAlignmentEntry "NC_0123:123-540" "AATATATATTTTTTTTTTTTTTTTTTTTTTAAAAAAAAAAAAAAAAAAAAAAATTTTTTTTTTTTTTTTTTTTT--------------------------------------------------------------------------",ClustalAlignmentEntry "NC_0223:123-540" "AATATATATTTTTTTTTTTTTTTTTTTTTTAAAAAAAAAAAAAAAAAAAAAAATTTTTTTTTTTTTTTTTTTTTAATATATATTTTTTTTTTTTTTTTTTTTTTAAAAAAAAAAAAAAAAAAAAAAATTTTTTTTTTTTTTTTTTTTT",ClustalAlignmentEntry "NC_0323:123-540" "AATATATATTTTTTTTTTTTTTTTTTTTTTAAAAAAAAAAAAAAAAAAAAAAATTTTTTTTTTTTTTTTTTTTTAATATATATTTTTTTTTTTTTTTTTTTTTTAAAAAAAAAAAAAAAAAAAAAAATTTTTTTTTTTTTTTTTTTTT"] "**************************************************************************                                              *                           "

structuralClustalParseError :: ParseError
structuralClustalParseError = addErrorMessage (Expect "\"mLocARNA\"") (newErrorMessage structuralClustalErrorMessage structuralClustalErrorSourcePosition)

structuralClustalErrorMessage :: Message
structuralClustalErrorMessage = SysUnExpect "\"i\""

structuralClustalErrorSourcePosition :: SourcePos
structuralClustalErrorSourcePosition = newPos "genParserStructuralClustalAlignment" 1 1

structuralClustalExample :: String
structuralClustalExample = "mLocARNA --- multiple Local (and global) Alignment of RNA --- LocARNA 1.8.0\nCopyright Sebastian Will\n\nCompute pair probs ...\nPerform progressive alignment ...\n\n\n\nAB001721.1/2735-2851  CCCGGUGACUAUAGAGAGAGGGCCACACCCGUUCCCAUCCCGAACACGGAAGUUAAGCCUCUCAUCGCUGAUGGUACUAUGUGGUUCGCUGCAUGGGAGAGUAGGACGUUGCCGGGU\ngi|451991584:1-117    CCCGGUGACUAUAGAGAGAGGGCCACACCCGUUCCCAUCCCGAACACGGAAGUUAAGCCUCUCAUCGCUGAUGGUACUAUGUGGUUCGCUGCAUGGGAGAGUAGGACGUUGCCGGGU\nalifold               (((((((((....(.(((.(((.....))).))).)...(((....)))..(((..(((((((((.((.(.(((.(((....))))))).))))))))).))..)))))))))))). (-38.30 = -38.30 +   0.00)\n"

structuralClustalResult :: StructuralClustalAlignment
structuralClustalResult = StructuralClustalAlignment [ClustalAlignmentEntry "AB001721.1/2735-2851" "CCCGGUGACUAUAGAGAGAGGGCCACACCCGUUCCCAUCCCGAACACGGAAGUUAAGCCUCUCAUCGCUGAUGGUACUAUGUGGUUCGCUGCAUGGGAGAGUAGGACGUUGCCGGGU",ClustalAlignmentEntry "gi|451991584:1-117" "CCCGGUGACUAUAGAGAGAGGGCCACACCCGUUCCCAUCCCGAACACGGAAGUUAAGCCUCUCAUCGCUGAUGGUACUAUGUGGUUCGCUGCAUGGGAGAGUAGGACGUUGCCGGGU"] "(((((((((....(.(((.(((.....))).))).)...(((....)))..(((..(((((((((.((.(.(((.(((....))))))).))))))))).))..))))))))))))." (-38.30)
