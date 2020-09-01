
module Parser.Util
  ( thenP
  , returnP
  , happyError
  , Parser
  , AlexPosn
  , AlexState(..)
  , lexer
  , runAlex
  , runLexer
  , runParser
  , syms
  )
  where

----------------------------------------------------------------------------
import Parser.Token
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Short as Short
import qualified Data.ByteString as Strict
----------------------------------------------------------------------------

-- For readablity - these are the interfaces Happy expects:

type Parser a = Alex a

thenP :: Parser a -> (a -> Parser b) -> Parser b
thenP = (>>=)

runParser :: Parser a -> String -> Either String a
runParser p s = runAlex s p

-- for testing purposes
runLexer :: String -> Either String [Token]
runLexer s = runParser tokens s
  where
    tokens :: Parser [Token]
    tokens = do
      t <-  tokenP
      if t == Token EOF then
        pure [t]
      else do
        ts <- tokens
        pure (t : ts)
      

tokenP :: Parser Token
tokenP = lexer pure

returnP :: a -> Parser a
returnP = return

alexShowError :: (Show t, Show t1) => (t, t1, Maybe String) -> Alex a
alexShowError (line, column, e) = alexError $ "show-error: " ++ (show (line, column, e))

alexGetPosition :: Alex (AlexPosn)
alexGetPosition = Alex $ \s@AlexState{alex_pos=pos} -> Right (s, pos)

happyError :: Parser a
happyError = do
  (AlexPn _ line col) <- alexGetPosition
  alexShowError (line, col, Nothing)

lexer :: (Token -> Parser a) -> Parser a
lexer = scanTokenM



