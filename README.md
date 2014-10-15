ClustalParser   [![Build Status](https://travis-ci.org/eggzilla/ClustalParser.svg)](https://travis-ci.org/eggzilla/ClustalParser)
=============

Currently contains parsers and datatypes for: clustalw2, clustalo

Clustal tools are multipe sequence aligenment tools for biological sequences 
like DNA, RNA and Protein.
For more information on clustal Tools refer to <http://www.clustal.org/>.


3 types of output are produced

 - Alignment file (.aln): 
  * Parsing with readClustalAlignment from filehandle (Bio.ClustalParser)
  * Parsing with parseClustalAlignment from string (Bio.ClustalParser)

 - Summary (printed to STDOUT):
  * Parsing with readClustalSummary from filehandle (Bio.ClustalParser)
  * Parsing with parseClustalSummary from string (Bio.ClustalParser)

 - Phylogenetic Tree (.dnd):
  * Parsing with readGraphNewick from filehandle (Bio.Phylogeny)
  * Parsing with readGraphNewick from String (Bio.Phylogeny)
