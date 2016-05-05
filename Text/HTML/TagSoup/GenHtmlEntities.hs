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
import Text.Printf
import Data.Maybe
import Text.Regex.Base
import Text.Regex.TDFA.Text
import Text.HTML.TagSoup.Entity

-- wget http://www.w3.org/TR/html5/entities.json
entitiesFN = "Text/HTML/TagSoup/entities.json"
readEntitiesJSON = do
    Just (JSON.Object o) <-
        fmap JSON.decode $ BL.readFile entitiesFN
    let e (JSON.Object (HM.lookup "codepoints" -> Just (JSON.Array a))) =
            map c $ V.toList a
        c (JSON.Number i) = toEnum $ truncate i
        strip (n, x) = (T.dropWhile (== '&') n, x)
        noSemicolon x = fromMaybe x $ T.stripSuffix ";" x
        semicolonFirst x
            | Just x' <- T.stripSuffix ";" x = (x', 0)
            | otherwise = (x, 1)
    return $
        -- sorting it the same way it sorted in entities.json
        sortBy (comparing $
                \ (x,_) -> (T.toLower $ noSemicolon x, semicolonFirst x)) $
        map strip $ HM.toList $ HM.map e o
readEntitiesOrderedAsIs = do
    f <- readFile entitiesFN
    return $
       flip mapMaybe (lines f) $ \ l -> do
        -- "&Aacute;": { "codepoints": [193], "characters": "\u00C1" },
        [[_, entity, codepoints]] <- return $
            regexGet (makeRegex ("&([^\"]+).*(\\[.+\\])" :: String)) l
        return (T.pack $ entity, map toEnum $ read codepoints)

regexGet :: Regex -> String -> [[String]]
regexGet r t = match r t

w3cEntities = do
    entitiesOrderedAsIs <- readEntitiesOrderedAsIs
    entitiesJSON <- readEntitiesJSON
--     when (HM.fromList entitiesOrderedAsIs /= HM.fromList entitiesJSON) $
--         fail "Ivalid parse result"
--     print (take 20 $ zipWith (\ (a,_) (b,_) -> (a,b)) entitiesOrderedAsIs entitiesJSON)
    when (entitiesOrderedAsIs /= entitiesJSON) $
        fail "Different order?"

    let entities =
--            sortBy (comparing $ \ (e,_) -> HM.lookup e tsOrder) $
            entitiesOrderedAsIs
        tsOrder = HM.fromList $ zip (map (T.pack . fst) htmlEntities) [0..]
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
        putStrLn $ "    ,\"" ++ T.unpack n ++ "\" * " ++
--             "\"" ++ concatMap (printf "\\x%04X") x ++ "\""
            --  ^ for TagSoup patch
            (if x `elem` needEscape then
                 show x -- ++ " -- escaped"
             else
                 "\"" ++ x ++ "\"" ++
                 (if length x > 1 then " -- " ++ show x else ""))
    print ("max entity length", maximum $ map (T.length . fst) entities)
    print ("max result characters", maximum $ map (length . snd) entities)
    print
        [ e | e@(n,x) <- entities
        , B.length (T.encodeUtf8 $ T.pack x) > (T.length n+2) ]
    -- [("nGt","\8811\8402"),("nLt","\8810\8402")]
    -- The only two entities that are longer in UTF-8 code than &; encoded.
    -- They become 6 characters instead of 5 when encoded in UTF-8.
    -- But unescapeHtml reserves 2x space (10 bytes for "&nGt;")
    -- so everything is still safe.
