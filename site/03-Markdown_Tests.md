# To Do

* ~~Module name changes. I'll use `esdoc.erl`~~
* Coverage
* ~~Block quotes~~
* ~~Images~~
* ~~Sizes in images
  This is part of a larger problem. Should I allow inline HTML?~~
  Solved with the `\<\<\<...>>>` notation
  
* Internal Links -- we want to refer to one part of a document from another
* ~~display lists~~
* Pretty printing of code
* Compatability with github markdown

<<<{node,img,#{src=>"./newton.jpg", alt=>"newton", width => 600},[]}>>> 

# Inline code

Inside `code` blocks (ie those written inside ``\`'' marks escape
conventions are not obeyed. The content is not recursively scanned.

# Markdown test suite.

This file will contains a set of test for my markdown processor.  It
also documents **by example** how to write markdown code.

The output from the processor is either HTML or a set of ``fomatting
objects'' suitable for input to _Apache FOP_. Apache FOP is used to
transform the formatting objects into PDF.

**Instead of using XSLT to perfrm the transformnation we do
all the Tree -> tree transformations directly in Erlang.**

# Notes

[note]
  This should just make a `div` with `class='note'`.

# Line breaks in paragraphs

This is the last^
line in a paragraph.


# Horizontal lines

----

The Formatting Elements
====

Quotes
----

``quotes are easy,'' He said, &ldquo;I hope that's right!&rdquo;

## Links

HTTP links are written `[google](http://www.google.com/)`
[google](http://www.google.com/) and when you
__click on the link__ you go to the web address.

## Inlines

**(TWO STARTS) yellow ** set of tests. *(ONE STAR)* _ONE UNDERLINE_
__TWO underlines__. ~~Strike this text~~.

Sometime we want some `code face'd variable`.

## Lists

+ Unordered lists start with a plus
+ They are easy to write
+ like this

* Lists startng with star are like this
* this is another

## Description lists

This is item1 - a DT
  This is DD para 1. Items are left aligned with
  column 1. The first line of the Item is offset by N-blanks. The
  ``body'' of the definition is terminated by the first line with non
  blank data in the cutter.

  This is DD para 2.  LorLorem ipsum dolor sit amet, consectetur
  adipiscing elit, sed do eiusmod tempor incididunt ut labore et
  dolore magna aliqua. Ut enim ad minim veniam, quis nostrud
  exercitation ullamco laboris nisi ut aliquip ex ea commodo
  consequat. Duis aute irure dolor in


More Items - DT
  DD You can have several items in a display list.

# Parameter blocks

---
file: this
date: today
---


# Images

The notation for an image is `\!\[joe\](./joe.jpg)`

![joe](./joe.jpg)

<<<{node,img,#{width=>100,src=>"./joe.jpg"},[]}>>>


## Fenced Blocks

## Inline Liks

# Preformatted

    This is preformatted line 1
    line2
    last line

This is a regular paragraph

```
This is a preformatted fenced block
line 2
last line
```

# Images



# Block quotes

```
> The github convention of
making this into
a block quote
is rather nice.
```

> The github convention of
making this into
a block quote
is rather nice.

     
# Block Quotes

* Block quotes
  This is a paragraph1.

  LorLorem ipsum dolor sit amet, consectetur adipiscing elit, sed do
  eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim
  ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut
  aliquip ex ea commodo consequat. Duis aute irure dolor in
  reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla
  pariatur. Excepteur sint occaecat cupidatat non proident, sunt in
  culpa qui officia deserunt mollit anim id est laborum. 

  * line 1 
       LorLorem ipsum dolor sit amet, consectetur adipiscing elit,
       sed do eiusmod tempor incididunt ut labore et dolore magna
       aliqua. Ut enim ad minim veniam, quis nostrud exercitation
       ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis
       aute irure dolor in reprehenderit in voluptate velit esse
       cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat
       cupidatat non proident, sunt in culpa qui officia deserunt
       mollit anim id est laborum. r
 

  * line 2

  LorLorem ipsum dolor sit amet, consectetur adipiscing elit, sed do
  eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim
  ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut
  aliquip ex ea commodo consequat. Duis aute irure dolor in
  reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla
  pariatur. Excepteur sint occaecat cupidatat non proident, sunt in
  culpa qui officia deserunt mollit anim id est laborum. r

# Links and anchors

In pinciple we should be able to link to an anchor where the anchor is __anywhere in the page__
but I'm not sure if this is a good idea. Perhapse only h1 amd h2 should be targets of anchors.


# XML Parse trees

XML data is represented by the type

    +type tree() = {node, Tag::atom(), Attributes #{Key::atom() => Value::string()}, [tree()]} |
                   Char::integer()}

# Deliberaate errors

THis has **a deliberate error so the coverage check will detect this

# On the representation of HTML

    +type html() = 
         {node,Tag,#{}, [html()]}
       | {enode,Tag}
       | Integer

# When do character entities get expanded?

Character entities written `\&amp;XXX;` are expand on input and replaced by
the unicode characters they represent. If we want a literal `\&lt;` we
have to backquote it like this `\\\&amp;lt;`. The backslash in front of the
`\&amp;` stops the character entites from being expanded and `\&amp;lt;` is
output to HTML.

# What's the deal with `enode`? 

An ``enode'' is an _empty node_. `{enode,Tag}` is formatted as
`\&lt;Tag/>` and NOT `\&lt;Tag>\&lt;/Tag>`

Why? - it's due to the empty `pre` problem. `{node,pre,#{},\[]}` is an
empty `pre` node which you might think could be formmated as
 `\&lt;pre/\>` but this generates an error. So an empty `pre` node MUST
BE FORMATTED as `\&lt;pre>\&lt;/pre>`.

An empty pre node must be formatted as `\&lt;pre>\&lt;/pre>` and not 
`\&lt;pre/>`

An empty br node must be formatted as `\&lt;br/>` or `\&lt;br>`and not
`\&lt;br>\&lt;/br>`

