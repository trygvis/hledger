module Hledger.Read.Format (
          parseFormatString
        , formatValue
        , FormatString(..)
        , Field(..)
        , tests
        ) where

import Numeric
import Data.Maybe
import Test.HUnit
import Text.ParserCombinators.Parsec
import Text.Printf

{-
%[-][MIN WIDTH][.MAX WIDTH]EXPR

%-P     a transaction's payee, left justified
%20P    The same, right justified, at least 20 chars wide
%.20P   The same, no more than 20 chars wide
%-.20P  Left justified, maximum twenty chars wide
-}

data Field =
    Account         -- %A
  | DefaultDate     -- %D
  | Payee           -- %P
  | Total           -- %T
  | DepthSpace      -- %_
    deriving (Show, Eq)

data FormatString =
    FormatLiteral String
  | FormatField 
    Bool            -- Left justified 
    (Maybe Int)     -- Min width
    (Maybe Int)     -- Max width
    Field           -- Field
{-
  | FormatExpression 
    Bool            -- Left justified 
    (Maybe Int)     -- Min width
    (Maybe Int)     -- Max width
    String          -- EXPR
-}
    deriving (Show, Eq)

formatValue :: Bool -> Maybe Int -> Maybe Int -> String -> String
formatValue leftJustified min max value = printf formatS value
    where
      l = if leftJustified then "-" else ""
      min' = maybe "" show min
      max' = maybe "" (\i -> "." ++ (show i)) max
      formatS = "%" ++ l ++ min' ++ max' ++ "s"

parseFormatString :: String -> Either String [FormatString]
parseFormatString input = case parse formatStrings "(unknown)" input of
    Left y -> Left $ show y
    Right x -> Right x

{-
Parsers
-}

text :: Parser Char
text = letter

field :: Parser Field
field = do
    l <- oneOf ("ADPT")
    return $ case l of
        'A' -> Account
        'D' -> DefaultDate
        'P' -> Payee
        'T' -> Total
        _ -> error $ "Unknown field" ++ [l]

formatField :: Parser FormatString
formatField = do
    char '%'
    leftJustified <- optionMaybe (char '-')
    minWidth <- optionMaybe (many1 $ digit)
    maxWidth <- optionMaybe (do char '.'; many1 $ digit)
    field <- field
    return $ FormatField (isJust leftJustified) (parseDec minWidth) (parseDec maxWidth) field
    where
      parseDec s = case s of
        Just text -> Just m where ((m,_):_) = readDec text
        _ -> Nothing

formatLiteral :: Parser FormatString
formatLiteral = do
    s <- many1 c
    return $ FormatLiteral s
    where
      c =     noneOf "%"
          <|> try (string "%%" >> return '%')

formatString :: Parser FormatString
formatString =
        formatField
    <|> formatLiteral

formatStrings = many formatString

testFormat :: FormatString -> String -> String -> Assertion
testFormat fs value expected = assertEqual name expected actual
    where
        (name, actual) = case fs of
            FormatLiteral l -> ("literal", formatValue False Nothing Nothing l)
            FormatField leftJustify min max _ -> ("field", formatValue leftJustify min max value)

testParser :: String -> [FormatString] -> Assertion
testParser s expected = case (parseFormatString s) of
    Left  error -> assertFailure $ show error
    Right actual -> assertEqual ("Input: " ++ s) expected actual

tests = test [ formattingTests ++ parserTests ]

formattingTests = [
      testFormat (FormatLiteral " ") "" " "
    , testFormat (FormatField False Nothing Nothing Payee) "payee"      "payee"
    , testFormat (FormatField False (Just 10) Nothing Payee) "payee"    "     payee"
    , testFormat (FormatField False Nothing (Just 10) Payee) "payee"    "payee"
    , testFormat (FormatField True Nothing (Just 10) Payee) "payee"     "payee"
    , testFormat (FormatField True (Just 10) Nothing Payee) "payee"     "payee     "
    , testFormat (FormatField True (Just 10) (Just 10) Payee) "payee"   "payee     "
    , testFormat (FormatField True Nothing (Just 3) Payee) "payee"      "pay"
    ]

parserTests = [
      testParser ""             []
    , testParser "P"            [FormatLiteral "P"]
    , testParser "%P"           [FormatField False Nothing Nothing Payee]
    , testParser "%T"           [FormatField False Nothing Nothing Total]
    , testParser "Hello %P!"    [FormatLiteral "Hello ", FormatField False Nothing Nothing Payee, FormatLiteral "!"]
    , testParser "%-P"          [FormatField True Nothing Nothing Payee]
    , testParser "%20P"         [FormatField False (Just 20) Nothing Payee]
    , testParser "%.10P"        [FormatField False Nothing (Just 10) Payee]
    , testParser "%20.10P"      [FormatField False (Just 20) (Just 10) Payee]
    , testParser "%20A %.10T\n" [ FormatField False (Just 20) Nothing Account
                                , FormatLiteral " "
                                , FormatField False Nothing (Just 10) Total
                                , FormatLiteral "\n"
                                ]
  ]
