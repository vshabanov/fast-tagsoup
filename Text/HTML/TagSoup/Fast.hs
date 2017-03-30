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
import Text.HTML.TagSoup.Entity (htmlEntities)
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
import qualified Data.Map as Map

instance IsString Word8 where
    fromString [c] = B.c2w c
    fromString _ = error "fromString :: Word8"

type P = Ptr Word8
r :: P -> Word8
r ptr = unsafePerformIO (peek ptr)
{-# INLINE r #-}
ri :: P -> Int -> Word8
ri ptr i = unsafePerformIO (peek (ptr `plusPtr` i))
{-# INLINE ri #-}

pp :: Ptr Word8 -> Ptr Word8
pp !x = x `plusPtr` 1
pp2 :: Ptr Word8 -> Ptr Word8
pp2 !x = x `plusPtr` 2
pp3 :: Ptr Word8 -> Ptr Word8
pp3 !x = x `plusPtr` 3
p1 :: Int -> Int
p1 !x = succ x
mm :: Int -> Int
mm !y = pred y
{-# INLINE mm #-}
{-# INLINE pp #-}
{-# INLINE pp2 #-}
{-# INLINE pp3 #-}
{-# INLINE p1 #-}

-- toLowerBS = B.map toLower
-- toLower is inplace
toLowerBS :: B.ByteString -> B.ByteString
toLowerBS bs@(B.PS fp offs len) =
    unsafePerformIO $ withForeignPtr fp $ \ p -> do
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
parseTags s = unsafePerformIO $ withForeignPtr fp $ \ p ->
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
          -- https://dev.w3.org/html5/spec-preview/tokenization.html#tokenizing-character-references
          -- TODO: more correct character reference tokenization
          -- there are additional rules for attributes too.

          -- The right way is here
          -- http://www.w3.org/TR/html5/tokenization.html
          -- https://dev.w3.org/html5/spec-preview/tokenization.html
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
              | l >= 8 && r p == "<" && ri p 1 == "/" &&
                (ri p 2 == "s" || ri p 2 == "S") &&
                (ri p 3 == "c" || ri p 3 == "C") &&
                (ri p 4 == "r" || ri p 4 == "R") &&
                (ri p 5 == "i" || ri p 5 == "I") &&
                (ri p 6 == "p" || ri p 6 == "P") &&
                (ri p 7 == "t" || ri p 7 == "T")
                -- it seems that there is no much difference
                -- between nextIC and low level code
--              | nextIC p l (toLowerUpperPairs "</script") =
                = mkText False l n : tagName False (p `plusPtr` 8) (l - 8) 6
              | otherwise = script (pp p) (mm l) (p1 n)
          style :: P -> Int -> Int -> [Tag B.ByteString]
          style _ 0 0 = []
          style _ 0 n = [mkText False 0 n]
          style !p !l !n
              | l >= 7 && r p == "<" && ri p 1 == "/" &&
                (ri p 2 == "s" || ri p 2 == "S") &&
                (ri p 3 == "t" || ri p 3 == "T") &&
                (ri p 4 == "y" || ri p 4 == "Y") &&
                (ri p 5 == "l" || ri p 5 == "L") &&
                (ri p 6 == "e" || ri p 6 == "E")
--              | nextIC p l (toLowerUpperPairs "</style") =
                = mkText False l n : tagName False (p `plusPtr` 7) (l - 7) 5
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
          tagName !o !p !left !n
              | left == 0 = [tag] -- output tag on EOF before closing bracket
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
              | alpha (r p) = tagName True (pp p) (mm l) 2
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
              | otherwise = attName t (pp p) (mm l) 1
          attName !t !p !l !n
              | l == 0 = [addAttr t (mkS l n) ""] -- "<a href"
              | space (r p) = afterAttName t (mkS l n) (pp p) (mm l)
              | r p == ">" = tdat (addAttr t (mkS l n) "") (pp p) (mm l)
              | r p == "?" || r p == "/" =
                  selfClosingStartTag (addAttr t (mkS l n) "") (pp p) (mm l)
              | r p == "=" = beforeAttValue t (mkS l n) (pp p) (mm l)
              | otherwise = attName t (pp p) (mm l) (p1 n)
          afterAttName !t !a !p !l
              | l == 0 = [addAttr t a ""] -- "<a href "
              | space (r p) = afterAttName t a (pp p) (mm l)
              | r p == "=" = beforeAttValue t a (pp p) (mm l)
              | r p == ">" = tdat (addAttr t a "") (pp p) (mm l)
              | r p == "?" || r p == "/" =
                  selfClosingStartTag (addAttr t a "") (pp p) (mm l)
              | otherwise = attName (addAttr t a "") (pp p) (mm l) 1
          beforeAttValue !t !a !p !l
              | l == 0 = [addAttr t a ""] -- "<a href=" or "<a href= "
              | space (r p) = beforeAttValue t a (pp p) (mm l)
              | r p == ">" = tdat (addAttr t a "") (pp p) (mm l)
--               | (r p == "?" || r p == "/") &&
--                 (l == 0 || (r (pp p) == ">"))  =
--                   selfClosingStartTag (addAttr t a "") (pp p) (mm l)
              | r p == "'" || r p == "\"" = attValue (r p) t a (pp p) (mm l) 0
              | otherwise = attValueUnquoted t a (pp p) (mm l) 1
          attValue !end !t !a !p !l !n
              | l == 0 = [addAttr t a (mkS l n)] -- "<a href='..."
              | r p == end = beforeAttName (addAttr t a (mkS l n)) (pp p) (mm l)
              | otherwise = attValue end t a (pp p) (mm l) (p1 n)
          attValueUnquoted !t !a !p !l !n
              | l == 0 = [addAttr t a (mkS l n)] -- "<a href=..."
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
          closeTag !t@(TagOpen !tn _) !rs
              -- B.length tn > 1 && B.head tn == '?' = t rs
              -- better to make separate parsing for <?xml ...> tag
              -- Chrome just skips from "<?" to ">"
              -- <?anything asdf="qwer>zxcv"?> becomes zxcv"?>
              -- it conforms to spec
              -- https://dev.w3.org/html5/spec-preview/tokenization.html#bogus-comment-state
              -- but we use ?xml as a tag and look for encoding parameter
              = t : TagClose tn : rs
          closeTag !t !rs = t : rs
          space 0x20 = True
          space !n = n >= 9 && n <= 13 -- \t\n\v\f\r
          alphaBQ !c = alpha c || c == "?" || c == "!"

alpha, alphaNum :: Word8 -> Bool
alpha c = (c >= "a" && c <= "z") || (c >= "A" && c <= "Z")
alphaNum c = alpha c || (c >= "0" && c <= "9")
hex, decimal :: Word8 -> Maybe Int
hex c
    | c >= "0" && c <= "9" = Just $ fromEnum (c - "0")
    | c >= "a" && c <= "f" = Just $ fromEnum (c - "a" + 10)
    | c >= "A" && c <= "F" = Just $ fromEnum (c - "A" + 10)
    | otherwise = Nothing
decimal c
    | c >= "0" && c <= "9" = Just $ fromEnum (c - "0")
    | otherwise = Nothing

-- ord maxBound = 1114111 = 0x10FFFF = &#1114111; &#x10FFFF;
-- &
--   # (X or x) hexDigit
--     any number of hex digits
--       if result accumulator becomes more than (maxBound :: Char)
--       further characters are skipped
--       and result become 0xFFFD -- replacement character
--
--       any non hex digits means end, ';' at the end must be ignored
--   # decimalDigit
--     any number of decimal digits
--       same maxBound and ';' logic
--   alpha
--     up to (maxHtmlEntityLength-1) alphanum characters
--       lookup entities without trailing ';' -- exit if found, ignoring ';'
--
--       finish on non alphanum, and lookup entity with trailng ';'
--       if finished on ';'

-- | Alternative to 'unescapeHtml' working with 'Text'
unescapeHtmlT :: T.Text -> T.Text
unescapeHtmlT s
    | Nothing <- T.find (== '&') s = s
    | otherwise = bst $ unescapeHtml $ T.encodeUtf8 s

-- | Convert escaped HTML to raw.
unescapeHtml :: B.ByteString -> B.ByteString
unescapeHtml src
    | not (B8.elem "&" src) = src
    | B.length s' == 0      = src
      -- ignore newly created string when there is no changes
    | otherwise             = s'
    where s' = unsafePerformIO $ withForeignPtr fp $ \ s ->
               B.createAndTrim (len*2) $ \ dst -> do
                     (ch, dst') <- go (s `plusPtr` offset) dst len False
                     return $ if ch then dst' `minusPtr` dst else 0
          (fp, offset, len) = B.toForeignPtr src
          go :: P -> P -> Int -> Bool -> IO (Bool, P)
          go !_ !d !0 !c = return (c, d)
          go !s !d !l !c
              | r s /= "&" =
                  poke d (r s) >> go (pp s) (pp d) (mm l) c
              | otherwise = entityStart (pp s) d (mm l) c
          entityStart !s !d !l !c
              | l == 0 = poke d "&" >> return (c, pp d)
              | r s == "#" && l >= 3 && (r (pp s) == "x" || r (pp s) == "X")
              , Just x <- hex (r $ pp2 s) =
                  baseEntity hex 16 (pp3 s) d (l-3) x
              | r s == "#" && l >= 2
              , Just x <- decimal (r $ pp s) =
                  baseEntity decimal 10 (pp2 s) d (l-2) x
              | alpha (r s) =
                  entity (pp s) d (mm l) (maxHtmlEntityLength-1) [r s] c
              | otherwise = poke d "&" >> go s (pp d) l c
          baseEntity char base !s !d !l !acc
              | l == 0 = putChar d acc $ \ d' -> return (True, d')
              | Just x <- char (r s)
              , acc' <- acc*base + x =
                  if acc' > ord maxBound then
                      overflowBaseEntity char (pp s) d (mm l)
                  else
                      baseEntity char base (pp s) d (mm l) acc'
              | otherwise = putChar d acc $ \ d' -> skipSemicolon s d' l True
          overflowBaseEntity char !s !d !l
              | l == 0 = putChar d overflowChar $ \ d' -> return (True, d')
              | Just _ <- char (r s) =
                  overflowBaseEntity char (pp s) d (mm l)
              | otherwise =
                  putChar d overflowChar $ \ d' -> skipSemicolon s d' l True
          overflowChar = ord '\xFFFD'
          -- Chrome/Safari/FF use this as replace character for entities
          -- that are too large.
          putChar d acc next = do
              let put !d [] = next d
                  put !d (x:xs) =
                      poke d x >> put (pp d) xs
              put d $ encodeChar $ chr acc
          skipSemicolon !s !d !l !c
              | l == 0 = return (c, d)
              | r s == ";" = go (pp s) d (mm l) True
              | otherwise = go s d l c
          entity !s !d !l !n !acc !c
              | n == 0 || l == 0 =
                  notEntity
              | alphaNum (r s)
              , n >= maxHtmlEntityLength - maxNoSemicolonHtmlEntityLength
              , Just e <- lookupNoSemicolonHtmlEntityRev (r s : acc) =
                  putEntity skipSemicolon e
                  -- Entities without semicolon have at least 2 characters,
                  -- so it's OK to check them starting on the 2nd character
                  -- (first one is already in `acc` on first call to `entity`).
                  -- And we need to check them before putting to `acc`
                  -- since such entity is complete no matter what characters
                  -- (ot maybe EOF) is coming later.
              | alphaNum (r s) =
                  entity (pp s) d (mm l) (mm n) (r s : acc) c
              | r s == ";"
              , Just e <- lookupSemicolonHtmlEntityRev acc =
                  -- all these entities require ';'.
                  -- entities that do not are already checked so it's safe
                  -- to go to notEntity if there is not ';' at the end
                  putEntity go e
              | otherwise =
                  notEntity
              where putEntity next e = do
                        let put !d [] = next (pp s) d (mm l) True
                            put !d (x:xs) =
                                poke d x >> put (pp d) xs
                        put d e
                    notEntity =
                        poke d "&" >>
                        go (s `plusPtr` (n-maxHtmlEntityLength))
                           (pp d) (l+maxHtmlEntityLength-n) c

lookupNoSemicolonHtmlEntityRev :: [Word8] -> Maybe [Word8]
lookupNoSemicolonHtmlEntityRev = \x -> Map.lookup x mp
    where mp = Map.fromList [ (map B.c2w $ reverse e
                              ,concatMap encodeChar x)
                            | (e,x) <- htmlEntities, last e /= ';']

lookupSemicolonHtmlEntityRev :: [Word8] -> Maybe [Word8]
lookupSemicolonHtmlEntityRev = \x -> Map.lookup x mp
    where mp = Map.fromList [ (map B.c2w $ tail $ reverse e
                              ,concatMap encodeChar x)
                            | (e,x) <- htmlEntities, last e == ';']

maxHtmlEntityLength :: Int -- = 32
maxHtmlEntityLength = maximum $ map (length . fst) htmlEntities

maxNoSemicolonHtmlEntityLength :: Int -- = 6
maxNoSemicolonHtmlEntityLength =
    maximum [length e | (e,_) <- htmlEntities, last e /= ';']

-- minHtmlEntityLength :: Int -- = 2
-- minHtmlEntityLength = minimum $ map (length . fst) htmlEntities

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

bst :: B.ByteString -> T.Text
bst = T.decodeUtf8With (\ _ -> fmap B.w2c)

-- | Alternative to 'parseTags' working with 'Text'
parseTagsT :: B.ByteString -> [Tag T.Text]
parseTagsT = map textTag . parseTags

textTag :: Tag B.ByteString -> Tag T.Text
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
    where s' = unsafePerformIO $ withForeignPtr fp $ \ src ->
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

-- | Show a list of tags, as they might have been parsed.
renderTags :: [Tag B.ByteString] -> B.ByteString
renderTags = renderTags' escapeHtml B.concat

-- | Alternative to 'renderTags' working with 'Text'
renderTagsT :: [Tag T.Text] -> T.Text
renderTagsT = renderTags' escapeHtmlT T.concat

renderTags' :: (Eq a, IsString a) => (a -> a) -> ([a] -> t) -> [Tag a] -> t
renderTags' escape concat = go []
    where go acc [] = concat $ reverse acc
          go acc (TagOpen "br" _ : TagClose "br" : ts) =
              go ("<br/>" : acc) ts
          go acc (TagOpen t as : ts) =
              (if t == "script" || t == "style" then goUnescaped else go)
              (">" : renderAtts (reverse as) (t : "<" : acc)) ts
          go acc (TagClose t : ts) =
              go (">" : t : "</" : acc) ts
          go acc (TagText t : ts) =
              go (escape t : acc) ts
          go acc (_ : ts) = go acc ts -- make compiler happy
          goUnescaped acc (TagText t : ts) =
              go (t : acc) ts
          goUnescaped acc ts = go acc ts
          renderAtts [] rs = rs
          renderAtts ((a,""):as) rs =
              a : " " : renderAtts as rs
          renderAtts ((a,v):as) rs =
              "\"" : escape v : "=\"" : a : " " : renderAtts as rs

-- | Decode XML to UTF-8 using @encoding@ attribute of @\<?xml\>@ tag.
ensureUtf8Xml :: B.ByteString -> B.ByteString
ensureUtf8Xml s
    | Right _ <- T.decodeUtf8' s = s
      -- first of all we try decode utf-8.
      -- Some sites specify non utf-8 encoding, while the text is in utf.
    | otherwise =
    case dropWhile isText $ parseTags s of
        --         ^ sometimes there is a space or junk text before <?xml>
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
    where isText (TagText _) = True
          isText _ = False
