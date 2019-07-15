module Parser where

import LispVal
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L
import Debug.Trace

type Parser = Parsec Void String

symbol = L.symbol sc
lexeme = L.lexeme sc

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
 where
  lineCmnt  = L.skipLineComment ";"
  blockCmnt = L.skipBlockCommentNested "#|" "|#"

specialChar :: Parser Char
specialChar = oneOf "|!$%&*+-/:<=>?@^_~"

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many (noneOf "\"")
  _ <- char '"'
  return $ String x

parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseBool :: Parser LispVal
parseBool = do
    _ <- char '#'
    (symbol "t" >> return (Bool True)) <|> (symbol "f" >> return (Bool False))

parseAtom :: Parser LispVal
parseAtom = do
  first <- letterChar <|> specialChar
  rest  <- many (alphaNumChar <|> specialChar)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = Number . read <$> (parsePositive <|> parseNegative)
 where
  parsePositive = some digitChar
  parseNegative = (:) '-' <$> (char '-' >> some digitChar)

parseListContents :: Parser LispVal
parseListContents = List <$> endBy parseExpr sc

parseDottedListContents :: Parser LispVal
parseDottedListContents = do
  x <- endBy parseExpr sc
  xs <- symbol "." >> L.lexeme sc parseExpr
  return $ DottedList x xs

parseList' :: Parser LispVal -> Parser LispVal
parseList' = between (symbol "(") (symbol ")")

parseLists :: Parser LispVal
parseLists = parseList' $ parseListContents <|> parseDottedListContents

parseExpr :: Parser LispVal
parseExpr =
    parseString
    <|> try parseNumber
    <|> parseAtom
    <|> parseBool
    <|> parseQuoted
    <|> (symbol "..." >> return Elipsis) -- TODO: parseElipsis
    <|> between (symbol "(") (symbol ")") (try parseDottedListContents <|> parseListContents)

parseTopLevelExpr :: Parser [LispVal]
parseTopLevelExpr = endBy (lexeme parseExpr) sc

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lsh" input of
    Left err  -> throwError $ Default (errorBundlePretty err)
    Right val -> return val

readExpr :: String -> ThrowsError [LispVal]
readExpr = readOrThrow (between sc eof parseTopLevelExpr)

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow $ between sc eof (many (between sc sc parseExpr))

