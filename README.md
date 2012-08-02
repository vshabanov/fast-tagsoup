fast-tagsoup
============

Fast Haskell tagsoup parser.

Speeds of 20-200MB/sec were observed.

Works only with strict bytestrings.

This library is intended to be used in conjunction with the original <code>tagsoup</code> package:

<pre>
import Text.HTML.TagSoup hiding (parseTags, renderTags)
import Text.HTML.TagSoup.Fast
</pre>

Besides speed <code>fast-tagsoup</code> correctly handles HTML <code>&lt;script&gt;</code> and <code>&lt;style&gt;</code> tags, converts tags to lower case and can decode non UTF-8 XML for you.

This parser is used in production in BazQux Reader feeds and comments crawler.

Use <pre>cabal install fast-tagsoup</pre> to get it.

