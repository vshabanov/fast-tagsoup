{-# LANGUAGE ViewPatterns #-}
module Text.HTML.TagSoup.GenHtmlEntities where

import qualified Data.Map as Map
import qualified Data.Aeson as JSON
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.List
import Control.Monad
import Data.Ord
import Data.Char

-- wget http://www.w3.org/TR/html5/entities.json
w3cEntities = do
    Just (JSON.Object o) <-
        fmap JSON.decode $ BL.readFile "Text/HTML/TagSoup/entities.json"
    let e (JSON.Object (HM.lookup "codepoints" -> Just (JSON.Array a))) =
            map c $ V.toList a
        c (JSON.Number i) = toEnum $ truncate i
        strip (n, x) =
            (-- T.dropWhileEnd (== ';') $
             T.dropWhile (== '&') n, x)
        check a b
            | a /= b = error $ show (a, b)
            | otherwise = a
        entities =
            sortBy (comparing $ \ (x,_) -> (T.toLower x, x)) $
            -- HM.toList $ HM.fromListWith check $
            map strip $ HM.toList $ HM.map e o
        needEscape =
            -- Haskell doesn't like these symbols unescaped
            [ "\\", "\"", "\n", "\t"
              -- all symbols below have generalCategory == Format
              -- There is total of 150 of such symbols but not all of them
              -- are used in HTML entities
            , "\173" -- shy
            , "\8203" -- NegativeMediumSpace, NegativeThickSpace,
                      -- NegativeThinSpace, NegativeVeryThinSpace,
                      -- ZeroWidthSpace
            , "\8204" -- zwnj
            , "\8205" -- zwj
            , "\8206" -- lrm
            , "\8207" -- rlm
            , "\8288" -- NoBreak
            , "\8289" -- ApplyFunction, af
            , "\8290" -- InvisibleTimes, it
            , "\8291" -- InvisibleComma, ic
            ]
    forM_ entities $ \ (n, x) ->
        putStrLn $ "    , \"" ++ T.unpack n ++ "\" * " ++
            (if x `elem` needEscape then
                 show x ++ " -- escaped "
             else
                 "\"" ++ x ++ "\"" ++
                 (if length x > 1 then " -- " ++ show x else ""))
    print ("max entity length", maximum $ map (T.length . fst) entities)
    print ("max result characters", maximum $ map (length . snd) entities)
    print
        [ e | e@(n,x) <- entities
        , B.length (T.encodeUtf8 $ T.pack x) > (T.length n+2) ]
    -- [("nGt","\8811\8402"),("nLt","\8810\8402")]
    -- the only two that is longer in direct code than encoded
    -- become 6 characters when encoded in UTF-8 but
    -- unescapeHtml reserves 2x space (10 bytes for "&nGt;")
    -- so it's safe

import Text.HTML.TagSoup.Fast
testUnescapeHtml :: IO ()
testUnescapeHtml = sequence_ tests >> putStrLn "OK"
    where tests =
              [t "&" "&"
              ,t "&&" "&&"
              ,t "&a" "&a"
              ,t "&am" "&am"
              ,t "&amp" "&"
              ,t "&amp1" "&1"
              ,t "&#" "&#"
              ,t "&#1" "\1"
              ,t "&&#1&am" "&\1&am"
              ,t "&&#1;&amp" "&\1&"
              ,t "&#amp" "&#amp"
              ,t "&#x1d505" "𝔅"
              ,t "&#1d505" "\1d505"
              ,t "&#1;d505" "\1d505"
              ,t "&#1114111" "\1114111"
              ,t "&#1114112" "\xFFFD"
              ,t "&#x10FFFF" "\x10FFFF"
              ,t "&#x110000" "\xFFFD"
              ,t "&#x1100001234567890z" "\xFFFDz"
              ,t "&#x1100001234567890;z" "\xFFFDz"
              ,t "rock&amproll &microm" "rock&roll µm"
              ,t "rock&amp;roll &micro;m" "rock&roll µm"
              ,t "&vsubnE;&CounterClockwiseContourIntegral;&aacute" "⫋︀∳á"
              ,t (T.pack $ concat ["a&" ++ e
                                  |(e,x) <- htmlEntities, last e /= ';'])
                 (T.pack $ concat ["a"++x
                                  |(e,x) <- htmlEntities, last e /= ';'])

              ]
          t a b
              | unescapeHtmlT a == b = return ()
              | otherwise = fail $ show (a, unescapeHtmlT a, b)
