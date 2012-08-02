fast-tagsoup
============

Fast Haskell tagsoup parser.

Speeds of 20-200MB/sec were observed.

Works only with strict bytestrings.

This library is intended to be used in conjunction with the original <tt>tagsoup</tt> package:

<pre>
import Text.HTML.TagSoup hiding (parseTags, renderTags)
import Text.HTML.TagSoup.Fast
</pre>

Besides speed <tt>fast-tagsoup</tt> correctly handles HTML <tt>&lt;script&gt;</tt> and <tt>&lt;style&gt;</tt> tags, converts tags to lower case and can dett non UTF-8 XML for you.

This parser is used in production in <a href="http://bazqux.com">BazQux Reader</a> feeds and comments crawler.

Use <tt>cabal install fast-tagsoup</tt> to get it.

