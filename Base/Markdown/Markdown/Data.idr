module Markdown.Data

import Data.String
import Data.List

import Extra.String

-- Note: this is currently infinitely recursive
-- %default total

public export
data Inline
  = Text String
  | Pre String
  | CodeBlock String (Maybe String)
  | Italics (List Inline)
  | Bold (List Inline)
  | Link String String
  | Image String String
  | Html String (List Inline)

public export
data Block
   = Header Nat (List Inline)
   | Paragraph (List Inline)

public export
data Markdown =
  Doc (List Block)

%name Markdown markdown

mutual
  showInlines : List Inline -> String
  showInlines = showList showInline

  showBlocks : List Block -> String
  showBlocks = showList showBlock

  showBlock : Block -> String
  showBlock (Header level inline) = "H" ++ (show level) ++ " " ++ (showInlines inline)
  showBlock (Paragraph inline) = "P " ++ (showInlines inline)

  showInline : Inline -> String
  showInline (Text text) = "Text " ++ (quote text)
  showInline (Pre text) = "Pre " ++ (quote text)
  showInline (CodeBlock text type) = "CodeBlock [" ++ show type ++ "] " ++ (quote text)
  showInline (Italics inline) = "Italics " ++ (showInlines inline)
  showInline (Bold inline) = "Bold " ++ (showInlines inline)
  showInline (Link href desc) = "Link " ++ href ++ " " ++ desc
  showInline (Image alt src) = "Image " ++ alt ++ " " ++ src
  showInline (Html tag inline) = "HTML <" ++ tag ++ "> " ++ (showInlines inline)

export
Show Inline where
  show = showInline

export
Show Block where
  show = showBlock

export
Show Markdown where
  show (Doc blocks) = showBlocks blocks

export
Eq Inline where
  (Text text0) == (Text text1) = text0 == text1
  (Pre text0) == (Pre text1) = text0 == text1
  (CodeBlock text0 type0) == (CodeBlock text1 type1) = text0 == text1 && type0 == type1
  (Italics inline0) == (Italics inline1) = inline0 == inline1
  (Bold inline0) == (Bold inline1) = inline0 == inline1
  (Link href0 desc0) == (Link href1 desc1) = href0 == href1 && desc0 == desc1
  (Image href0 desc0) == (Image href1 desc1) = href0 == href1 && desc0 == desc1
  (Html tag0 inline0) == (Html tag1 inline1) = tag0 == tag1 && inline0 == inline1
  _ == _ = False

export
Eq Block where
  (Header level0 inline0) == (Header level1 inline1) = level0 == level1 && inline0 == inline1
  (Paragraph inline0) == (Paragraph inline1) = inline0 == inline1
  _ == _ = False

export
Eq Markdown where
  (Doc a) == (Doc b) = a == b
