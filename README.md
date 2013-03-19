Yet Another Haskell Markdown Parser
==========

And not a very good one. Deviates from the specification in some ways,
some features are not implemented. I'll document these shortcomings
more extensively soon.

Usage
=========

showProcessed $ parseMarkdown "test string"

Actual input/output hopefully coming soon.

Shortcomings
=========

+ No links or images yet.
+ No nested lists, or block-type Markdown in lists.
+ Unordered lists can only begin with a +, no - or \*.
+ No escaping of characters, only limited encoding.
+ May not give the correct parse every time. I don't have a test suite yet, so.
+ Probably a lot of other things I haven't seen because I haven't tested extextensively. But I did it. I wrote a Markdown parser-ish using only ReadP combinators. That was a bad idea.
