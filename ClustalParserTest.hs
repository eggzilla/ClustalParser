module Main where
    
import System.Environment (getArgs)
import System.IO
import System.Environment
import Bio.ClustalParser
import Data.Either
    
main = do
  args <- getArgs
  let input_file = (head args)                                     
  -- read Clustal outputfile
  parsedinput <- readClustalAlignment input_file
  print parsedinput
 
