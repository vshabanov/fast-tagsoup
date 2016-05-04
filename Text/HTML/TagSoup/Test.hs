import Text.HTML.TagSoup hiding (parseTags, renderTags)
import Text.HTML.TagSoup.Fast
import Text.HTML.TagSoup.HtmlEntities
import qualified Data.Text as T

type Test a = IO a

pass :: Test ()
pass = return ()

runTest :: Test () -> IO ()
runTest x = x >> putStrLn "All tests passed"

(===) :: (Show a, Eq a) => a -> a -> IO ()
a === b = if a == b then pass else fail $ "Does not equal: " ++ show a ++ " =/= " ++ show b

parseTests :: Test ()
parseTests = do
--    parseTags "<!DOCTYPE TEST>" === [TagOpen "!DOCTYPE" [("TEST","")]]
    parseTags "<!DOCTYPE TEST>" === [TagOpen "doctype" [("test","")]]
    -- lower case, should I add '!' to 'doctype' ?
    parseTags "<test \"foo bar\">" === [TagOpen "test" [("\"foo",""),("bar\"","")]]
--    parseTags "<test baz \"foo\">" === [TagOpen "test" [("baz",""),("\"foo\"","")]]
    -- FIXME
    parseTags "<test baz \"foo\">" === [TagOpen "test" [("baz","foo")]] -- ????
    parseTags "<test 'foo bar'>" === [TagOpen "test" [("'foo",""),("bar'","")]]
    parseTags "<test bar=''' />" === [TagOpen "test" [("bar",""),("'","")], TagClose "test"]
    parseTags "<test2 a b>" === [TagOpen "test2" [("a",""),("b","")]]
    parseTags "<test2 ''>" === [TagOpen "test2" [("''","")]]
    parseTags "</test foo>" === [TagClose "test"]
    parseTags "<test/>" === [TagOpen "test" [], TagClose "test"]
    parseTags "<test1 a = b>" === [TagOpen "test1" [("a","b")]]
    parseTags "hello &amp; world" === [TagText "hello & world"]
    parseTags "hello &#64; world" === [TagText "hello @ world"]
    parseTags "hello &#x40; world" === [TagText "hello @ world"]
    parseTags "hello &#X40; world" === [TagText "hello @ world"]
    parseTags "hello &haskell; world" === [TagText "hello &haskell; world"]
    parseTags "hello \n\t world" === [TagText "hello \n\t world"]
    parseTags "<a href=http://www.google.com>" === [TagOpen "a" [("href","http://www.google.com")]]
    parseTags "<foo bar=\"bar&#54;baz\">" === [TagOpen "foo" [("bar","bar6baz")]]
    parseTags "<foo bar=\"bar&amp;baz\">" === [TagOpen "foo" [("bar","bar&baz")]]
    parseTags "hey &how are you" === [TagText "hey &how are you"]
    parseTags "hey &how; are you" === [TagText "hey &how; are you"]
    parseTags "hey &amp are you" === [TagText "hey & are you"]
    parseTags "hey &amp; are you" === [TagText "hey & are you"]

    -- real cases reported by users
    parseTagsT "&nwarr;x&ngeqq;" === [TagText "\x2196x\x2267\x0338"]
    parseTagsT "test &#10933649; test" === [TagText "test \xFFFD test"]

    parseTags "<a href=\"series.php?view=single&ID=72710\">" === [TagOpen "a" [("href","series.php?view=single&ID=72710")]]

--    parseTags "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">" ===
--        [TagOpen "!DOCTYPE" [("HTML",""),("PUBLIC",""),("","-//W3C//DTD HTML 4.01//EN"),("","http://www.w3.org/TR/html4/strict.dtd")]]
--        [TagOpen "doctype" [("html",""),("public",""),("","-//W3C//DTD HTML 4.01//EN"),("","http://www.w3.org/TR/html4/strict.dtd")]]
--  FIXME

    parseTags "<script src=\"http://edge.jobthread.com/feeds/jobroll/?s_user_id=100540&subtype=slashdot\">" ===
        [TagOpen "script" [("src","http://edge.jobthread.com/feeds/jobroll/?s_user_id=100540&subtype=slashdot")]]

    parseTags "<a title='foo'bar' href=correct>text" === [TagOpen "a" [("title","foo"),("bar'",""),("href", "correct")],TagText "text"]

    parseTags "<test><![CDATA[Anything goes, <em>even hidden markup</em> &amp; entities]]> but this is outside</test>" ===
--        [TagOpen "test" [],TagText "Anything goes, <em>even hidden markup</em> &amp; entities but this is outside",TagClose "test"]
    -- two `TagText`s
        [TagOpen "test" [],TagText "Anything goes, <em>even hidden markup</em> &amp; entities", TagText " but this is outside",TagClose "test"]

    parseTags "<a \r\n href=\"url\">" === [TagOpen "a" [("href","url")]]

    parseTags "<a href='random.php'><img src='strips/130307.jpg' alt='nukular bish'' title='' /></a>" ===
        [TagOpen "a" [("href","random.php")],TagOpen "img" [("src","strips/130307.jpg"),("alt","nukular bish"),("'",""),("title","")],TagClose "img",TagClose "a"]

    parseTags "<p>some text</p\n<img alt='&lt; &yyy; &gt;' src=\"abc.gif\">" ===
        [TagOpen "p" [],TagText "some text",TagClose "p"]

    parseTags "<script> if (x<bomb) </script>" === [TagOpen "script" [], TagText " if (x<bomb) ", TagClose "script"]
    parseTags "<script> if (x<bomb) " === [TagOpen "script" [], TagText " if (x<bomb) "]
--    parseTags "<SCRIPT language=foo> if (x<bomb) </SCRIPT>" === [TagOpen "SCRIPT" [("language","foo")], TagText " if (x<bomb) ", TagClose "SCRIPT"]
    -- lower case
    parseTags "<SCRIPT language=foo> if (x<bomb) </SCRIPT>" === [TagOpen "script" [("language","foo")], TagText " if (x<bomb) ", TagClose "script"]
    parseTags "<script /><test>" === [TagOpen "script" [], TagClose "script", TagOpen "test" []]

    -- some escapes require trailing semicolons, see #28 and #27.
    parseTagsT "one &mid; two" === [TagText "one \8739 two"]
    parseTagsT "one &mid two" === [TagText "one &mid two"]
    parseTagsT "one &micro; two" === [TagText "one \181 two"]
    parseTagsT "one &micro two" === [TagText "one \181 two"]

    testUnescapeHtml

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
