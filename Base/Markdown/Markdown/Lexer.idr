module Markdown.Lexer

import Data.List1

import Text.Lexer
import Text.Token

import Markdown.String
import public Markdown.Tokens

%default total

private
markdownTokenMap : TokenMap MarkdownToken
markdownTokenMap = toTokenMap $
  [ (codeFence, MdCodeBlock)
  , (pre, MdPre)
  , (headingSym, HeadingSym)
  , (italicsSym, ItalicsSym)
  , (boldSym, BoldSym)
  , (imageSym, ImageSym)
  , (link, MdLink)
  , (htmlCloseTag, HtmlCloseTag)
  , (htmlOpenTag, HtmlOpenTag)
  , (newLine, NewLine)
  , (text, MdText)
  ]

||| Combine consecutive `MdText` nodes into one
combineText : List MarkdownToken -> List MarkdownToken
combineText [] = []
combineText (el :: rest) =
  let
    init = (List1.singleton el, el)
  in
    forget $ reverse $ fst $ (foldl accumulate init rest)
  where
    accumulate : (List1 MarkdownToken, MarkdownToken) -> MarkdownToken -> (List1 MarkdownToken, MarkdownToken)
    accumulate (acc0 ::: acc1, last) el =
      case (last, el) of
        (Tok MdText a, Tok MdText b) =>
          let
            combined = Tok MdText (a ++ b)
          in
          (combined ::: acc1, combined)
        _ =>
          (el ::: acc0 :: acc1, el)

public export
lexMarkdown : String -> Maybe (List MarkdownToken)
lexMarkdown str
  = case lex markdownTokenMap str of
         (tokens, _, _, "") => Just $ combineText $ map TokenData.tok tokens
         _ => Nothing
