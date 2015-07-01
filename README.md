ClustalParser [![Hackage](https://img.shields.io/hackage/v/ClustalParser.svg)](https://hackage.haskell.org/package/ClustalParser) [![Build Status](https://travis-ci.org/eggzilla/ClustalParser.svg)](https://travis-ci.org/eggzilla/ClustalParser)
=============

Currently contains parsers and datatypes for: clustalw2, clustalo, mlocarna

Clustal tools are multiple sequence alignment tools for biological sequences 
like DNA, RNA and Protein.
For more information on clustal Tools refer to <http://www.clustal.org/>.

Mlocarna is a multiple sequence alignment tool for RNA sequences with
secondary structure output. 
For more information on mlocarna refer to <http://www.bioinf.uni-freiburg.de/Software/LocARNA/>.

4 types of output are parsed

 - Alignment file (.aln): 
  * Parsing with readClustalAlignment from filepath (Bio.ClustalParser)
  * Parsing with parseClustalAlignment from String (Bio.ClustalParser)
  
 - Alignment file with secondary structure (.aln): 
  * Parsing with readStructuralClustalAlignment from filepath (Bio.ClustalParser)
  * Parsing with parsStructuralClustalAlignment from String (Bio.ClustalParser)

 - Summary (printed to STDOUT):
  * Parsing with readClustalSummary from filepath (Bio.ClustalParser)
  * Parsing with parseClustalSummary from String (Bio.ClustalParser)

 - Phylogenetic Tree (.dnd):
  * Parsing with readGraphNewick from filepath (Bio.Phylogeny)
  * Parsing with readGraphNewick from String (Bio.Phylogeny)
