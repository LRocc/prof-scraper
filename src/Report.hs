{-
{-# LANGUAGE OverloadedStrings #-}

module Report
    (  generateReport
    ) where
    
import Control.Monad (forM_)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import ProfScrapeLib
import Text.Blaze.Html.Renderer.String (renderHtml)

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

generateReport :: [(String, Maybe Int)] -> String
generateReport = renderHtml . generatePage


generatePage :: [(String, Maybe Int)] -> Html
generatePage resultingProfsReport = H.docTypeHtml $ do
    H.head $ do
        H.h1 ! A.class_ "font-black text-3xl" $  "Report"
    H.body $ do
        --counts <- mapM numProfessors schools
        --let results = zip schools counts
        H.p "List of professors per school"
        H.table $ forM_ resultingProfsReport (H.tr . mapM_ (H.td . toHtml))
        
            
    -}        

