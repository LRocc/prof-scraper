{-# LANGUAGE OverloadedStrings #-}



module ProfScrapeLib
    (  numProfessors
    ) where
    
    --schools = ["ges","psychology","mathematicsstatistics","engineering","computing","chemistry","physics","humanities","law"]

import Text.HTML.Scalpel
import Data.List




numProfessors :: String -> IO (Maybe Int)
--numProfessors x = return Nothing
numProfessors x = do
  results <- scrapeURL (urlPlusStaff x) selectedStaff
  appendFile "some-file.txt" (show $ fmap (length . actualProfessor . concat) results)
  return $ fmap (length . actualProfessor . concat) results

  
{-
concatenation:: MyList a -> MyList a -> MyList a
concatenation Empty Empty = Empty
concatenation Empty bl@(Element b bs) = bl
concatenation al@(Element a as) Empty = al
concatenation (Element a as) bl@(Element b bs) = Element a (concatenation as bl)
-}

urlPlusStaff :: String -> String
urlPlusStaff string = "https://www.gla.ac.uk/schools/" ++ string ++ "/staff"

{--
scrapeAllItems :: String−> Maybe [String]
scrapeAllItems input = do
  scrapeStringLike input items
    where
      items :: Scraper String [String]
      items = texts "li"
--}

{--
comments :: Scraper String [Comment]
       comments = chroots ("div" @: [hasClass "container"]) comment
--}

--The @: operator creates a selector that matches a tag based on the name and various conditions on the tag's attributes
-- The texts function takes a selector and returns the inner text from every set of tags (possibly nested) matching the given selector.

selectedStaff :: Scraper String [[String]]
selectedStaff = chroots ("ul" @: [notP ("id" @= "honorary-visitinglist")]) $ texts "li"


{-
helper :: String -> String
helper = show . notP ("id" @= "honorary-visitinglist")
-}
{-
selectedStaff :: Scraper String [[String]]
selectedStaff = chroots ("ul" @: [helper]) . texts "li"
    where
        helper = notP ["id" @= "honorary-visitinglist"]

-}


f &&& g = \x -> f x && g x

actualProfessor :: [String] -> [String]
actualProfessor = filter $ professor &&& notAssistant &&& notAssociate
  where professor    =       isInfixOf "Professor"
        notAssistant = not . isInfixOf "Assistant Professor"
        notAssociate = not . isInfixOf "Associate Professor"

