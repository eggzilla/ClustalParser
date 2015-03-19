module Main where
    
import System.Environment (getArgs)
import Bio.ClustalParser

main :: IO ()
main = do
  args <- getArgs
  let input_file = (head args)                                     
  -- read Clustal outputfile
  parsedinput <- readStructuralClustalAlignment input_file
  print parsedinput
 
