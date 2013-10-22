-- {-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, BangPatterns, FlexibleInstances,
             ScopedTypeVariables, PatternGuards #-}
{-|
  Very fast TagSoup parser.

  Works only with strict bytestrings.
  Correctly handles HTML @\<script\>@ and @\<style\>@ tags.

  This module is intended to be used in conjunction with the original @tagsoup@ package:

  > import Text.HTML.TagSoup hiding (parseTags, renderTags)
  > import Text.HTML.TagSoup.Fast

  Remark that tags are returned in lower case and comments are not returned.

  In long running multithreaded applications it's generally recommended to use
  'parseTagsT' and work with @[@'Tag' 'Text'@]@ to reduce memory fragmentation.
-}
module Text.HTML.TagSoup.Fast
    ( parseTags, renderTags, parseTagsT, renderTagsT, ensureUtf8Xml
    , escapeHtml, escapeHtmlT, unescapeHtml, unescapeHtmlT
    )
    where

import Text.HTML.TagSoup (Tag(..))
import Text.HTML.TagSoup.Entity
import Text.StringLike (StringLike(..))

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString as B8

import Data.Bits
import Data.Word
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import System.IO.Unsafe

import Data.String
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.ICU.Convert as ICU
import qualified Control.Exception as E

instance IsString Word8 where
    fromString [c] = B.c2w c
    fromString _ = error "fromString :: Word8"

type P = Ptr Word8
r :: P -> Word8
r ptr = B.inlinePerformIO (peek ptr)
{-# INLINE r #-}
ri :: P -> Int -> Word8
ri ptr i = B.inlinePerformIO (peek (ptr `plusPtr` i))
{-# INLINE ri #-}

pp :: Ptr Word8 -> Ptr Word8
pp !x = x `plusPtr` 1
p1 :: Int -> Int
p1 !x = succ x
mm :: Int -> Int
mm !y = pred y
{-# INLINE mm #-}
{-# INLINE pp #-}
{-# INLINE p1 #-}

-- toLowerBS = B.map toLower
-- toLower is inplace
toLowerBS :: B.ByteString -> B.ByteString
toLowerBS bs@(B.PS fp offs len) =
    B.inlinePerformIO $ withForeignPtr fp $ \ p -> do
        let go !_ 0 = return bs
            go !o l = do
                w <- peekByteOff p o
                pokeByteOff p o (B.c2w $ toLower $ B.w2c w)
                go (o+1) (l-1)
        go offs len

-- | Parse a string to a list of tags.
--
-- > parseTags "<div>&amp;<script>x<y</script>" ==
-- >   [TagOpen "div" [],TagText "&",TagOpen "script" [],TagText "x<y",TagClose "script"]
--
parseTags :: B.ByteString -> [Tag B.ByteString]
parseTags s = B.inlinePerformIO $ withForeignPtr fp $ \ p ->
          return $
                 map fixTag $ filter (/= TagText "") $
                 dat (p `plusPtr` offset) len 0
    -- [TagText s]
    where (fp, offset, len) =
              B.toForeignPtr $ B.copy s -- copy due to inline toLower
          fixTag (TagOpen t a) =
              TagOpen t (map (\(n,v) -> (toLowerBS n, unescapeHtml v)) $
                             reverse a)
--          fixTag (TagText t) = TagText (unescapeHtml t)
          fixTag t = t
          -- The right way is here
          -- http://www.w3.org/TR/html5/tokenization.html
          -- and it is what tagsoup package tries to do.
          -- I've simplified part of states (especially comments
          -- which are stripped from ouput)
          mkS !left !n = B.fromForeignPtr fp (offset + len - left - n) n
          mkText unescape !left !n
              | unescape  = TagText $ unescapeHtml $ mkS left n
              | otherwise = TagText $ mkS left n
                            -- leave script/style/CDATA as is
          script :: P -> Int -> Int -> [Tag B.ByteString]
          script _ 0 0 = []
          script _ 0 n = [mkText False 0 n]
          script !p !l !n
              | l >= 9 && r p == "<" && ri p 1 == "/" &&
                (ri p 2 == "s" || ri p 2 == "S") &&
                (ri p 3 == "c" || ri p 3 == "C") &&
                (ri p 4 == "r" || ri p 4 == "R") &&
                (ri p 5 == "i" || ri p 5 == "I") &&
                (ri p 6 == "p" || ri p 6 == "P") &&
                (ri p 7 == "t" || ri p 7 == "T") &&
                ri p 8 == ">" =
                  -- it seems that there is no much difference
                  -- between nextIC and low level code
--                nextIC p l (toLowerUpperPairs "</script>") =
                  mkText False l n : TagClose "script"
                  : dat (p `plusPtr` 9) (l - 9) 0
              | otherwise = script (pp p) (mm l) (p1 n)
          style :: P -> Int -> Int -> [Tag B.ByteString]
          style _ 0 0 = []
          style _ 0 n = [mkText False 0 n]
          style !p !l !n
              | l >= 8 && r p == "<" && ri p 1 == "/" &&
                (ri p 2 == "s" || ri p 2 == "S") &&
                (ri p 3 == "t" || ri p 3 == "T") &&
                (ri p 4 == "y" || ri p 4 == "Y") &&
                (ri p 5 == "l" || ri p 5 == "L") &&
                (ri p 6 == "e" || ri p 6 == "E") &&
                ri p 7 == ">" =
--              | nextIC p l (toLowerUpperPairs "</style>") =
                  mkText False l n : TagClose "style"
                  : dat (p `plusPtr` 8) (l - 8) 0
              | otherwise = style (pp p) (mm l) (p1 n)

          dat :: P -> Int -> Int -> [Tag B.ByteString]
          dat _ 0 0 = []
          dat _ 0 n = [mkText True 0 n]
          dat !p !left !n
              | r p == "<" = mkText True left n : tagOpen (pp p) (mm left)
              | otherwise = dat (pp p) (mm left) (p1 n)
          tdat !t !p !l = t : case t of
              TagOpen "script" _ -> script p l 0
              TagOpen "style" _ -> style p l 0
              _ -> dat p l 0
          tagOpen p 0 = dat p 0 1
          tagOpen !p !left
              | r p == "!" = markupDeclOpen (pp p) (mm left)
              | alpha (r p) || r p == "?" = tagName True (pp p) (mm left) 1
              | r p == "/" = closeTagOpen (pp p) (mm left)
              | otherwise = dat p left 1
          closeTagOpen p 0 = dat p 0 2
          closeTagOpen !p !left
              | alphaBQ (r p) = tagName False (pp p) (mm left) 1
              | otherwise = dat p 0 2
          tagName _ _ 0 _ = [] -- ouput nothing on EOF before tag closing bracket
          tagName !o !p !left !n
              | space (r p) = beforeAttName tag (pp p) (mm left)
              | r p == ">" = tdat tag (pp p) (mm left)
              | r p == "?" || r p == "/" =
                  selfClosingStartTag tag (pp p) (mm left)
--              | r p == "'" || r p == "\"" = attValue (r p) tag "" (pp p) (mm left) 0
              | otherwise = tagName o (pp p) (mm left) (p1 n)
              where tag | o         = TagOpen  (toLowerBS $ mkS left n) []
                        | otherwise = TagClose (toLowerBS $ mkS left n)
          markupDeclOpen p 0 = dat p 0 2
          markupDeclOpen !p !l
              | alpha (r p) = tagName True (pp p) (mm l) 1
              | r p == "-" && l >= 2 && ri p 1 == "-" -- next p l "--"
                  = commentStart (p `plusPtr` 2) (l - 2) 0
              | l >= 7 && r p == "[" && ri p 1 == "C" && ri p 2 == "D"
                 && ri p 3 == "A" && ri p 4 == "T" && ri p 5 == "A"
                 && ri p 6 == "["
                  -- next p l "[CDATA["
                  = cdataSection (p `plusPtr` 7) (l - 7) 0
              | otherwise = dat p l 2
          beforeAttName t _ 0 = [t]
          beforeAttName !t !p !l
              | space (r p) = beforeAttName t (pp p) (mm l)
              | r p == ">" = tdat t (pp p) (mm l)
              | r p == "?" || r p == "/" = selfClosingStartTag t (pp p) (mm l)
--              | r p == "'" || r p == "\"" = attValue (r p) t "" (pp p) (mm l) 0
              | otherwise = attName t (pp p) (mm l) 1
          attName t _ 0 _ = [t]
          attName !t !p !l !n
              | space (r p) = afterAttName t (mkS l n) (pp p) (mm l)
              | r p == ">" = tdat (addAttr t (mkS l n) "") (pp p) (mm l)
              | r p == "?" || r p == "/" =
                  selfClosingStartTag (addAttr t (mkS l n) "") (pp p) (mm l)
              | r p == "=" = beforeAttValue t (mkS l n) (pp p) (mm l)
              | otherwise = attName t (pp p) (mm l) (p1 n)
          afterAttName t _ _ 0 = [t]
          afterAttName !t !a !p !l
              | space (r p) = afterAttName t a (pp p) (mm l)
              | r p == "=" = beforeAttValue t a (pp p) (mm l)
              | r p == ">" = tdat (addAttr t a "") (pp p) (mm l)
              | r p == "?" || r p == "/" =
                  selfClosingStartTag (addAttr t a "") (pp p) (mm l)
              | r p == "'" || r p == "\"" = attValue (r p) t a (pp p) (mm l) 0
              | otherwise = attName (addAttr t a "") (pp p) (mm l) 1
          beforeAttValue t _ _ 0 = [t]
          beforeAttValue !t !a !p !l
              | space (r p) = beforeAttValue t a (pp p) (mm l)
              | r p == ">" = tdat (addAttr t a "") (pp p) (mm l)
--               | (r p == "?" || r p == "/") &&
--                 (l == 0 || (r (pp p) == ">"))  =
--                   selfClosingStartTag (addAttr t a "") (pp p) (mm l)
              | r p == "'" || r p == "\"" = attValue (r p) t a (pp p) (mm l) 0
              | otherwise = attValueUnquoted t a (pp p) (mm l) 1
          attValue _ t _ _ 0 _ = [t]
          attValue !end !t !a !p !l !n
              | r p == end = beforeAttName (addAttr t a (mkS l n)) (pp p) (mm l)
              | otherwise = attValue end t a (pp p) (mm l) (p1 n)
          attValueUnquoted t _ _ 0 _ = [t]
          attValueUnquoted !t !a !p !l !n
              | space (r p) = beforeAttName (addAttr t a (mkS l n)) (pp p) (mm l)
              | r p == ">" --  || r p == "/"
                  = beforeAttName (addAttr t a (mkS l n)) p l
              | otherwise = attValueUnquoted t a (pp p) (mm l) (p1 n)
          commentStart _ 0 _ = [] -- we do not output comments
          commentStart !p !l !n
              | l >= 3 && r p == "-" && ri p 1 == "-" && ri p 2 == ">"
                  -- next p l "-->"
                  = dat (p `plusPtr` 3) (l - 3) 0
              | otherwise = commentStart (pp p) (mm l) (p1 n)
          cdataSection p 0 n = dat p 0 n
          cdataSection !p !l !n
              | l >= 3 && r p == "]" && ri p 1 == "]" && ri p 2 == ">"
                  -- next p l "]]>"
                  = mkText False l n : dat (p `plusPtr` 3) (l-3) 0
              | otherwise = cdataSection (pp p) (mm l) (p1 n)
          selfClosingStartTag t _ 0 = closeTag t []
          selfClosingStartTag !t !p !l
              | r p == ">" = closeTag t $ dat (pp p) (mm l) 0
              | otherwise = beforeAttName t p l
          addAttr (TagOpen !tn !ta) !a !v = TagOpen tn ((a, v):ta)
          addAttr t _ _ = t
          closeTag !t@(TagOpen !tn _) !rs = t : TagClose tn : rs
          closeTag !t !rs = t : rs
          space 0x20 = True
          space !n = n >= 9 && n <= 13 -- \t\n\v\f\r
          alpha !c = (c >= "a" && c <= "z") || (c >= "A" && c <= "Z")
          alphaBQ !c = alpha c || c == "?" || c == "!"

-- next  p l [] = True
-- next  p 0  _  = False
-- next !p !l (x:xs) = (r p == B.c2w x) && next (pp p) (mm l) xs

-- toLowerUpperPairs s = [(B.c2w $ toLower x, B.c2w $ toUpper x) | x <- s]

-- nextIC  p l [] = True
-- nextIC  p 0  _  = False
-- nextIC !p !l ((a,b):xs) = (r p == a || r p == b) && nextIC (pp p) (mm l) xs

{-# INLINE parseTags #-}

-- ord maxBound = 1114111 = 0x10FFFF = &#1114111; &#x10FFFF;
-- maximum 8 characters between '&' and ';'
-- entities length are also not longer than 8
-- so we are looking for ';' after '&' no more than 9 symbols

-- | Alternative to 'unescapeHtml' working with 'Text'
unescapeHtmlT :: T.Text -> T.Text
unescapeHtmlT s
    | Nothing <- T.find (== '&') s = s
    | otherwise = bst $ unescapeHtml $ T.encodeUtf8 s

-- | Convert escaped HTML to raw.
unescapeHtml :: B.ByteString -> B.ByteString
unescapeHtml s
    | not (B8.elem "&" s) = s
    | B.length s' == 0   = s -- ignore newly created string when there is no changes
    | otherwise          = s'
    where s' = B.inlinePerformIO $ withForeignPtr fp $ \ src ->
               B.createAndTrim (len*2) $ \ dst -> do
                     (ch, dst') <- go (src `plusPtr` offset) dst len False
                     return $ if ch then dst' `minusPtr` dst else 0
          (fp, offset, len) = B.toForeignPtr s
          go :: P -> P -> Int -> Bool -> IO (Bool, P)
          go !_ !d !0 !c = return (c, d)
          go !s !d !l !c
--              | r s .&. (0x8 + 0x4) == 0x8
              | r s /= "&" =
                  poke d (r s) >> go (pp s) (pp d) (mm l) c
              | otherwise = entity (pp s) d (mm l) 9 [] c
--           add !c !s !d !n !l =
--               poke d c >> go (s `plusPtr` n) (pp d) (l - n) True
          entity !s !d !l !n !acc !c
              | n == 0 || l == 0 =
                  poke d "&" >> go (s `plusPtr` (n-9)) (pp d) (l+9-n) c
              | r s /= ";" =
                  entity (pp s) d (mm l) (mm n) (B.w2c (r s) : acc) c
              | otherwise = case lookupEntity $ reverse acc of
                  Just [i] -> do
                      -- there are strange two character entities, some of them
                      -- are actually single character
                      -- (&Bfr; = '\x1D505', but returned as "\xD835\xDD05")
                      -- ignore them.
                      let put !d [] = go (pp s) d (mm l) True
                          put !d (x:xs) =
                              poke d x >> put (pp d) xs
                      put d $ encodeChar i
                  _ ->
                      poke d "&" >> go (s `plusPtr` (n-9)) (pp d) (l+9-n) c

encodeChar :: Char -> [Word8]
encodeChar = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `shiftR` 6)
                        , 0x80 + oc .&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `shiftR` 12)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `shiftR` 18)
                        , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]

bst = T.decodeUtf8With (\ _ -> fmap B.w2c)

-- | Alternative to 'parseTags' working with 'Text'
parseTagsT :: B.ByteString -> [Tag T.Text]
parseTagsT = map textTag . parseTags

textTag (TagOpen t a) = TagOpen (bst t) [(bst n, bst v) | (n,v) <- a]
textTag (TagClose t) = TagClose (bst t)
textTag (TagText t) = TagText (bst t)
textTag (TagComment t) = TagComment (bst t)
textTag (TagWarning t) = TagWarning (bst t)
textTag (TagPosition r c) = TagPosition r c

-- | Alternative to 'escapeHtml' working with 'Text'
escapeHtmlT :: T.Text -> T.Text
escapeHtmlT s
    | not $ T.any (\ c -> c=='&'||c=='<'||c=='>'||c=='\''||c=='"') s = s
    | otherwise = bst $ escapeHtml $ T.encodeUtf8 s

-- | Escape characters unsafe to HTML
escapeHtml :: B.ByteString -> B.ByteString
escapeHtml s
    | Nothing <- B8.find (\ c -> c=="&"||c=="<"||c==">"||c=="'"||c=="\"") s = s
    | B.length s' == 0 = s
    | otherwise        = s'
    where s' = B.inlinePerformIO $ withForeignPtr fp $ \ src ->
               B.createAndTrim (len*6) $ \ dst -> do
                     (ch, dst') <- go (src `plusPtr` offset) dst len False
                     return $ if ch then dst' `minusPtr` dst else 0

          (fp, offset, len) = B.toForeignPtr s
          go :: P -> P -> Int -> Bool -> IO (Bool, P)
          go !_ !d !0 !c = return (c, d)
          go !s !d !l !c
              | r s == "&" = add s d l $ map B.c2w "&amp;"
              | r s == "<" = add s d l $ map B.c2w "&lt;"
              | r s == ">" = add s d l $ map B.c2w "&gt;"
              | r s == "'" = add s d l $ map B.c2w "&apos;"
              | r s == "\"" = add s d l $ map B.c2w "&quot;"
              | otherwise =
                  poke d (r s) >> go (pp s) (pp d) (mm l) c
          add !s !d !l [] = go (pp s) d (mm l) True
          add !s !d !l (x:xs) = do
              poke d x
              add s (pp d) l xs

{-# INLINE unescapeHtml #-}

-- | Show a list of tags, as they might have been parsed.
renderTags = renderTags' escapeHtml B.concat
-- | Alternative to 'renderTags' working with 'Text'
renderTagsT = renderTags' escapeHtmlT T.concat

renderTags' escape concat = go []
    where go acc [] = concat $ reverse acc
          go acc (TagOpen "br" _ : TagClose "br" : ts) =
              go ("<br/>" : acc) ts
          go acc (TagOpen t as : ts) =
              go (">" : renderAtts (reverse as) (t : "<" : acc)) ts
          go acc (TagClose t : ts) =
              go (">" : t : "</" : acc) ts
          go acc (TagText t : ts) =
              go (escape t : acc) ts
          go acc (_ : ts) = go acc ts -- make compiler happy
          renderAtts [] rs = rs
          renderAtts ((a,v):as) rs =
              "\"" : escape v : "=\"" : a : " " : renderAtts as rs

{-# INLINE renderTags #-}

-- | Decode XML to UTF-8 using @encoding@ attribute of @\<?xml\>@ tag.
ensureUtf8Xml :: B.ByteString -> B.ByteString
ensureUtf8Xml s
    | Right _ <- T.decodeUtf8' s = s
      -- first of all we try decode utf-8.
      -- Some sites specify non utf-8 encoding, while the text is in utf.
    | otherwise =
    case parseTags $ B.dropWhile isSpace s of
        --           ^ sometimes there is a space before ?xml
        (TagOpen "?xml" attrs : _)
            | Just enc <- lookup "encoding" attrs
            , B.map toLower enc /= "utf-8" ->
                unsafePerformIO $
                    (do -- print enc
                        c <- ICU.open (toString enc) Nothing
                        let t = T.encodeUtf8 $ ICU.toUnicode c s
                        B.length t `seq` return t)
                    `E.catch`
                    \ (_ :: E.SomeException) -> return s
                      -- in case of errors try process as utf-8
--                 TL.fromChunks $
--                     map (ICU.toUnicode $ unsafePerformIO $
--                          ICU.open (toString enc) Nothing) $
--                     BL.toChunks s
--                IConv.convert (toString enc) "utf-8" s
                -- convertFuzzy works only on GNU
        _ -> s -- TL.decodeUtf8With (\ _ -> fmap B.w2c) s
             -- TL.lenientDecode s
