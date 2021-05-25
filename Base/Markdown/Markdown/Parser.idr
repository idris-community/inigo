module Markdown.Parser

import Markdown.Lexer
import Markdown.Data
import Text.Parser
import Text.Token
import Data.List
import Data.List1

import public Markdown.Tokens

-- Note: this is currently infinitely recursive
-- %default total

mutual
  private
  markdown : Grammar MarkdownToken False Markdown
  markdown =
    do
      els <- many (
              (map Just header)
          <|> (map Just paragraph)
          <|> (map (const Nothing) $ newLine)
        )
      pure $ Doc (mapMaybe id els)

  private
  inline : Grammar MarkdownToken True Inline
  inline =
    (
          text
      <|> pre
      <|> codeBlock
      <|> bold
      <|> italics
      <|> image
      <|> link
      <|> html
      <|> parts
    )

  -- TODO: Handle other incomplete parts
  parts : Grammar MarkdownToken True Inline
  parts =
    map (const $ Text "!") (match ImageSym)

  wrapInline : MarkdownTokenKind -> (List Inline -> a) -> Grammar MarkdownToken True a
  wrapInline sym tok =
    do
      ignore $ match sym
      contents <- some inline
      ignore $ match sym
      pure $ tok (forget contents)

  private
  header : Grammar MarkdownToken True Block
  header =
    do
      level <- match HeadingSym
      commit -- TODO: Should we commit here?
      contents <- many inline
      blockTerminal
      pure $ Header level contents

  blockTerminal : Grammar MarkdownToken False ()
  blockTerminal =
    (map (const ()) $ (some newLine)) <|>
    eof

  private
  newLine : Grammar MarkdownToken True ()
  newLine =
    map (const ()) (match NewLine)

  private
  paragraph : Grammar MarkdownToken True Block
  paragraph =
    do
      contents <- some inline
      blockTerminal
      pure $ Paragraph (forget contents)

  private
  text : Grammar MarkdownToken True Inline
  text =
    map Text (match MdText)

  private
  pre : Grammar MarkdownToken True Inline
  pre =
    map Pre (match MdPre)

  private
  codeBlock : Grammar MarkdownToken True Inline
  codeBlock =
    map (uncurry CodeBlock) (match MdCodeBlock)

  private
  bold : Grammar MarkdownToken True Inline
  bold =
    wrapInline BoldSym Bold

  private
  italics : Grammar MarkdownToken True Inline
  italics =
    wrapInline ItalicsSym Italics

  private
  image : Grammar MarkdownToken True Inline
  image =
    do
      match ImageSym
      r <- match MdLink
      buildImage r

  private
  buildImage : (String, String) -> Grammar MarkdownToken False Inline
  buildImage (alt, src) =
    pure $ Image alt src

  private
  link : Grammar MarkdownToken True Inline
  link =
    map (\(href, desc) => Link href desc) (match MdLink)

  private
  html : Grammar MarkdownToken True Inline
  html =
    do
      openTag <- match HtmlOpenTag
      contents <- many inline
      closer openTag -- TODO: Is this inefficient?
      pure $ Html openTag contents

  private
  closer : String -> Grammar MarkdownToken True ()
  closer tag =
    do
      closeTag <- match HtmlCloseTag
      checkTag closeTag tag

  private
  checkTag : String -> String -> Grammar MarkdownToken False ()
  checkTag x y =
    if x == y
      then pure ()
      else fail "tag mismatch"

export
parseMarkdown : List MarkdownToken -> Maybe Markdown
parseMarkdown toks = case parse markdown toks of
                      Right (j, []) => Just j
                      _ => Nothing
