module Markdown.Tokens

import Data.String
import Markdown.String
import Extra.String
import public Text.Token

safeSub : Nat -> Nat -> Nat
safeSub _ Z = Z
safeSub Z v = v
safeSub (S n) (S k) = safeSub n k

getLinkDesc : String -> (String, String)
getLinkDesc text =
  let
    (linkPart, descPart) = break (== ']') text
    linkLenFinal = safeSub 1 (length linkPart)
    descLenFinal = safeSub 3 (length descPart)
  in
    (
      substr 1 linkLenFinal linkPart,
      substr 2 descLenFinal descPart
    )

public export
data MarkdownTokenKind
  = HeadingSym
  | MdText
  | MdPre
  | MdCodeBlock
  | NewLine
  | ItalicsSym
  | BoldSym
  | ImageSym
  | MdLink
  | HtmlOpenTag
  | HtmlCloseTag

public export
Eq MarkdownTokenKind where
  (==) HeadingSym HeadingSym = True
  (==) MdText MdText = True
  (==) MdPre MdPre = True
  (==) MdCodeBlock MdCodeBlock = True
  (==) NewLine NewLine = True
  (==) ItalicsSym ItalicsSym = True
  (==) BoldSym BoldSym = True
  (==) ImageSym ImageSym = True
  (==) MdLink MdLink = True
  (==) HtmlOpenTag HtmlOpenTag = True
  (==) HtmlCloseTag HtmlCloseTag = True
  (==) _ _ = False

public export
MarkdownToken : Type
MarkdownToken = Token MarkdownTokenKind

dropEnds : Int -> String -> String
dropEnds n str =
  let
    len : Int
    len = cast $ length str
  in
    strSubstr n (len - 2 * n) str

public export
TokenKind MarkdownTokenKind where
  TokType HeadingSym = Nat
  TokType MdText = String
  TokType MdPre = String
  TokType MdCodeBlock = (String, Maybe String)
  TokType NewLine = ()
  TokType ItalicsSym = ()
  TokType BoldSym = ()
  TokType ImageSym = ()
  TokType MdLink = (String, String)
  TokType HtmlOpenTag = String
  TokType HtmlCloseTag = String

  tokValue HeadingSym x = length x
  tokValue MdText txt = txt
  tokValue MdPre txt = dropEnds 1 txt
  tokValue MdCodeBlock txt =
    case Extra.String.split '\n' (dropEnds 3 txt) of
      [] =>
        ("", Nothing)

      ("\n" :: x) =>
        (join "\n" x, Nothing)

      hd :: x =>
        (join "\n" x, Just hd)

  tokValue NewLine _ = ()
  tokValue ItalicsSym _ = ()
  tokValue BoldSym _ = ()
  tokValue ImageSym _ = ()
  tokValue MdLink text = getLinkDesc text
  tokValue HtmlOpenTag tag = (filter isAlphaNum tag) -- TODO: Improve quality
  tokValue HtmlCloseTag tag = (filter isAlphaNum tag) -- how to do this?

export
Show MarkdownToken where
  show (Tok HeadingSym l) = "HeadingSym " ++ (show l)
  show (Tok MdText txt) = "MdText " ++ (quote txt)
  show (Tok MdPre txt) = "MdPre " ++ (quote txt)
  show (Tok MdCodeBlock txt) = "MdCodeBlock " ++ (quote txt)
  show (Tok NewLine _) = "NewLine"
  show (Tok ItalicsSym _) = "ItalicsSym"
  show (Tok BoldSym _) = "BoldSym"
  show (Tok ImageSym _) = "ImageSym"
  show (Tok MdLink txt) = "MdLink " ++ (quote txt)
  show (Tok HtmlOpenTag tag) = "StartTag " ++ quote tag
  show (Tok HtmlCloseTag tag) = "EndTag " ++ quote tag

export
Eq MarkdownToken where
  (Tok HeadingSym l0) == (Tok HeadingSym l1) = l0 == l1
  (Tok MdText txt0) == (Tok MdText txt1) = txt0 == txt1
  (Tok MdPre txt0) == (Tok MdPre txt1) = txt0 == txt1
  (Tok MdCodeBlock txt0) == (Tok MdCodeBlock txt1) = txt0 == txt1
  (Tok NewLine _) == (Tok NewLine _) = True
  (Tok ItalicsSym _) == (Tok ItalicsSym _) = True
  (Tok BoldSym _) == (Tok BoldSym _) = True
  (Tok ImageSym _) == (Tok ImageSym _) = True
  (Tok MdLink txt0) == (Tok MdLink txt1) = txt0 == txt1
  (Tok HtmlOpenTag tag0) == (Tok HtmlOpenTag tag1) = tag0 == tag1
  (Tok HtmlCloseTag tag0) == (Tok HtmlCloseTag tag1) = tag0 == tag1
  _ == _ = False
