module Construction.Internal.Parser where

import           Construction.Internal.Types (Term (..))
import           Data.Text                   (pack)
import           Text.Parsec.Char            (char, digit, space)
import           Text.Parsec.Combinator      (between, many1)
import           Text.Parsec.Prim            (many, try, (<|>))
import           Text.Parsec.Text            (Parser)



termP :: Parser Term
termP = varP <|> appP <|> lamP <|> bracketP

varP :: Parser Term
varP =  (\x y -> Var $ pack (x:y)) <$> char 'x'
                                   <*> many digit

appP :: Parser Term
appP = try $ between (char '(') (char ')') $
       App <$> (termP <* many1 space)
           <*> termP

bracketP :: Parser Term
bracketP = try $ between (char '(') (char ')') $ termP

lamP :: Parser Term
lamP = try $ between (char '(') (char ')') $ 
	   Lam <$> ((\x -> pack x) <$> ((char '\\') *> nameP <* (char '.'))) 
	   <*> termP

nameP :: Parser String
nameP = (:) <$> char 'x' <*> many digit
