module Main where

import ProfScrapeLib ( numProfessors )
import Data.Csv ( encode )

import Data.ByteString.Lazy (writeFile)

schools :: [String]
schools = [
          "chemistry",
          "computing",
          "engineering",
          "ges",
          "mathematicsstatistics",
          "physics",
          "psychology"
         ]

main :: IO ()
main = do
     counts <- mapM numProfessors schools
     let results = zip schools counts
     mapM_ (\x -> putStrLn $ (fst x) ++ ": " ++ ((show.snd) x)) results
     --Write the output to a CSV file called professorCount.csv
     Data.ByteString.Lazy.writeFile "professorCount.csv" $ encode results
