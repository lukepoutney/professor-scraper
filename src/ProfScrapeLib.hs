{-# LANGUAGE OverloadedStrings #-}

module ProfScrapeLib 
    ( numProfessors
    ) where
import Text.HTML.Scalpel
    ( scrapeURL,
      chroots,
      html,
      texts,
      scrapeStringLike,
      (@:),
      (@=),
      notP,
      Scraper )
import Data.Maybe ( fromJust )
import Data.List ( isInfixOf )

--Get the name of a staff memeber from their associated hyperlink
scrapeAllStaff :: String -> Maybe [String]
scrapeAllStaff input = do
  scrapeStringLike input items
    where
        items :: Scraper String [String]
        items  = texts "a"

--Get all staff listings except honorary and visiting
scrapeStaff :: Scraper String [String]
scrapeStaff =
    chroots ("div" @: ["class"@="row standardContent tab", notP ("data-url-hash" @= "honorary-visiting") ]) scrapeName

scrapeName :: Scraper String String
scrapeName = do html "ul"

numProfessors :: String -> IO (Maybe Int)
numProfessors subject = do 
    staffSets <- scrapeURL ("https://www.gla.ac.uk/schools/"++subject++"/staff/") scrapeStaff
    --Check if any staff listings were retrieved from the scrape, returning Nothing if none are found
    if fromJust staffSets /= []
        then do
            --Combine the staff listings into a single list, then retieve the staff names 
            let staffNames = fromJust (scrapeAllStaff $ concat (fromJust staffSets) )
            --If a staff memeber is a professor then mark them as true within a boolean list
            let boolList = fmap ("Professor" `isInfixOf`) staffNames
            --Count the true values and return the result
            let counted = Just (sum $ map fromEnum boolList)
            return counted
    else return Nothing