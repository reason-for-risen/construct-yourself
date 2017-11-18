{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Tasks.GADT_1.GADTParser where

import           Data.Text              (pack)
import           Tasks.GADT_1.GADTExpr
import           Text.Parsec.Char       (char, digit, space, string)
import           Text.Parsec.Combinator (between, many1)
import           Text.Parsec.Language   (haskellDef)
import           Text.Parsec.Prim       (many, parseTest, try, (<|>))
import           Text.Parsec.Text       (Parser)
import           Text.Parsec.Token

iLitP :: Parser (Lit Int)
iLitP = try $ spacedP $ (ILit . read) <$> many1 digit

bLitP :: Parser (Lit Bool)
bLitP = let catchBool 'T' = BLit True
            catchBool 'F' = BLit False
        in try $ catchBool <$> spacedP (char 'T' <|> char 'F')

iiLitP :: Parser (Expr Int)
iiLitP = Lit <$> iLitP

bbLitP :: Parser (Expr Bool)
bbLitP = Lit <$> bLitP

addP :: Parser (Expr Int)
addP = let adP = (iiLitP <|> (bracketP addP) <|> (bracketP iiLitP))
        in try $ Add <$> (adP <* char '+') <*> parse

leqP :: Parser (Expr Bool)
leqP = try $ Leq <$> (parse <* char '<') <*> parse

andP :: Parser (Expr Bool)
andP = let anP = (leqP <|> bbLitP <|> (bracketP leqP) <|> (bracketP bbLitP) <|> (bracketP andP))
        in try $ And <$> (anP <* string "&&") <*> parse

spacedP :: Parser a -> Parser a
spacedP p = (many space *> p) <* many space

bracketP :: Parser a -> Parser a
bracketP p = try $ spacedP (between (char '(') (char ')') $ spacedP $ p)

class MyParse a where
  parse :: Parser (Expr a)


instance MyParse Int where
    parse = addP <|> iiLitP <|> (bracketP parse)

instance MyParse Bool where
    parse = andP <|> leqP <|> bbLitP <|> (bracketP parse)
