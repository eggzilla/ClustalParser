ClustalParser   [![Build Status](https://travis-ci.org/eggzilla/ClustalParser.svg)](https://travis-ci.org/eggzilla/ClustalParser)
=============

Currently contains parsers and datatypes for: clustalw2, clustalo

Clustal tools are multipe sequence aligenment tools for biological sequences 
like DNA, RNA and Protein.
For more information on clustal Tools refer to <http://www.clustal.org/>.


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
