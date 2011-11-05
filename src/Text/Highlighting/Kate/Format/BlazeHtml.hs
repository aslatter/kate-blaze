{- |
   Module      : Text.Highlighting.Kate.Format
   Copyright   : Copyright (C) 2008 John MacFarlane, 2011 Antoine Latter
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha 
   Portability : portable

Formatters that convert a list of annotated source lines to various output formats.
-}

module Text.Highlighting.Kate.Format.BlazeHtml
    ( formatAsHtml, FormatOption (..), defaultHighlightingCss ) where
import Text.Highlighting.Kate.Format ( FormatOption(..), defaultHighlightingCss)
import Text.Highlighting.Kate.Definitions

import Text.Blaze.Html5 (toHtml, toValue, Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.List (intersperse)
import Data.Monoid (mconcat)



-- | Format a list of highlighted @SourceLine@s as Html.
formatAsHtml :: [FormatOption]  -- ^ Options
             -> String          -- ^ Language
             -> [SourceLine]    -- ^ Source lines to format
             -> Html
formatAsHtml opts lang lines =
  let startNum = getStartNum opts
      numberOfLines = length lines
      code = 
          H.code ! A.class_ (toValue $ unwords ["sourceCode", lang]) $
          mconcat $ intersperse H.br (map (sourceLineToHtml opts) lines)
  in  if OptInline `elem` opts
         then code
         else if OptNumberLines `elem` opts
                 then let onClick =
                              "with (this.firstChild.style) { display = (display == '') ? 'none' : '' }"
                          nums = H.td
                                   ! A.class_ (toValue "lineNumbers")
                                   ! A.title  (toValue "Click to toggle line numbers")
                                   ! A.onclick (toValue onClick)
                                   $ H.pre
                                   $ mconcat
                                   $ intersperse H.br (map lineNum [startNum..(startNum + numberOfLines - 1)])
                          lineNum n = if OptLineAnchors `elem` opts
                                         then H.a ! A.id (toValue $ show n) $ toHtml (show n)
                                         else toHtml $ show n
                          sourceCode = H.td ! A.class_ (toValue "sourceCode")
                                       $ H.pre ! A.class_ (toValue "sourceCode")
                                       $ code
                      in  H.table ! A.class_ (toValue "sourceCode") $ H.tr $ mconcat [nums, sourceCode]
                 else H.pre ! A.class_ (toValue "sourceCode") $ code

applyAtts elem [] = elem
applyAtts elem (x:xs) = (elem ! x) `applyAtts` xs

labeledSourceToHtml :: [FormatOption] -> LabeledSource -> Html
labeledSourceToHtml _ ([], txt)    = toHtml txt
labeledSourceToHtml opts (labs, txt)  =
  if null attribs
     then toHtml txt
     else H.span `applyAtts` attribs $ toHtml txt
   where classes = unwords $
                   if OptDetailed `elem` opts
                      then map removeSpaces labs
                      else drop 1 labs  -- first is specific
         attribs = [A.class_ (toValue classes) | not (null classes)] ++
                   [A.title  (toValue classes) | OptTitleAttributes `elem` opts]

sourceLineToHtml :: [FormatOption] -> SourceLine -> Html
sourceLineToHtml opts contents =
  mconcat $ map (labeledSourceToHtml opts) contents

removeSpaces :: String -> String
removeSpaces = filter (/= ' ')

getStartNum :: [FormatOption] -> Int
getStartNum [] = 1
getStartNum (OptNumberFrom n : _) = n
getStartNum (_:xs) = getStartNum xs

