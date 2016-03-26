module Parser where

import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

spacey :: Parser ()
spacey = L.space (spaceChar >> return ()) (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")
lexeme = L.lexeme spacey
symbol = L.symbol spacey
braces = lexeme . between (string "{") (string "}")
brackets = lexeme . between (string "[") (string "]")
parens = lexeme . between (string "(") (string ")")
comma = symbol ","
commaSep = (flip sepBy1) comma

parseLit = parseString <|> parseNum <|> parseObj <|> parseBool <|> parseArray

parseString = parseDoubleQuotes <|> parseSingleQuotes
parseDoubleQuotes = lexeme $ do 
                      str <- between (string "\"") (string "\"") $ many $ satisfy (\x -> x /= '"')
                      return $ LString Double str
parseSingleQuotes = lexeme $ do
                      str <- between (string "'") (string "'") $ many $ satisfy (\x -> x /= '\'')
                      return $ LString Single str
parseNum = LNum `fmap` float
parseProp = do
  n <- parsePropName
  symbol ":"
  e <- parseExp 
  return (n,e)
parseObj = LObj `fmap` (braces $ commaSep $ parseProp)
parseBool = (symbol "true" >> return (LBool True)) <|> (symbol "false" >> return (LBool False))
parseArray = LArray `fmap` (brackets $ commaSep $ parseExp)

parseExp = parseBinOp <|> parseUnOp <|> parseTernIf <|> parseNew <|> parseDel <|> parseFuncall
           <|> parseEVar <|> parseENan <|> parseEInf <|> parseUndefined <|> parseAccess
           <|> parseFunDecl <|> parseAssign <|> parseAssignDec <|> parseAssignInc
           <|> parseThis

parseStmt = undefined
