{
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE BangPatterns #-}
module Parser.Token where

import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Short as Short
import Data.String as String
}

%wrapper "monad"

$digit = 0-9        -- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-
  $white+    ;
  "--".*     ;
  module {tokChar Module }
  \( {tokChar LeftRoundBracket   }   
  \) {tokChar RightRoundBracket  }
  \[ {tokChar LeftSquareBracket  }
  \] {tokChar RightSquareBracket }
  \{ {tokChar LeftCurlyBracket   }
  \} {tokChar RightCurlyBracket  }
  \@ {tokChar At                 }
  \, {tokChar Comma              }
  \- {tokChar Dash               }
  \= {tokChar Equals             }
  :  {tokChar Ann                }
  ★  {tokChar Kind               }
  \. {tokChar Dot                }
  \; {tokChar EndDef             }
  1  {tokChar One                }
  2  {tokChar Two                }
  β  {tokChar Beta               }
  δ  {tokChar IndAbsurd          }
  ς  {tokChar Symm               }
  ρ  {tokChar Rewrite            }
  φ  {tokChar Phi                }
  ∀  {tokChar ForAll             }
  Π  {tokChar Pi                 }
  ι  {tokChar Intersection       }
  λ  {tokChar Lam                }
  Λ  {tokChar BigLam             }
  ≃  {tokChar Eq                 }
  $alpha [$alpha $digit \_ \']*	{tok_app Name  }

{
tokChar
  :: TokenType
  -> (AlexPosn, a, [Byte], String)
  -> Int
  -> Alex Token
tokChar tokTy (pos, _, input, _) len = return
  $ Token tokTy

tok_app
  :: (Short.ShortByteString -> TokenType)
  -> (AlexPosn, a, [Byte], String)
  -> Int
  -> Alex Token
tok_app f (_, _, _, s) len = pure (Token s')
  where
 -- note this doesn't handle unicode correctly
    s' = Name . fromString . take len $ s


type Name = Short.ShortByteString

data Token = Token !TokenType
  deriving (Eq, Show)

-- The token type:
data TokenType =
  Module             |
  EndDef             |
  LeftRoundBracket   |
  RightRoundBracket  |
  LeftSquareBracket  |
  RightSquareBracket |
  LeftCurlyBracket   |
  RightCurlyBracket  |
  Name Name          |
  At                 |
  Comma              |
  Dash               |
  Equals             |
  Ann                |
  Kind               |
  Box                |
  Dot                |
  One                |
  Two                |
  Beta               |
  IndAbsurd          |
  Symm               |
  Rewrite            |
  Phi                |
  ForAll             |
  Pi                 |
  Intersection       |
  Lam                |
  BigLam             |
  Eq                 |
  EOF
  deriving (Eq,Show)


alexEOF :: Alex Token
alexEOF = do
--  (p, _, _, _) <- alexGetInput
  return $ Token EOF

scanToken :: String -> Either String Token
scanToken str = runAlex str alexMonadScan


scanTokenM :: (Token -> Alex a) -> Alex a
scanTokenM = (alexMonadScan >>=)

toShortByteString :: Lazy.ByteString -> Short.ShortByteString
toShortByteString = Short.toShort . Lazy.toStrict


syms :: String
syms = "()[]{}@,-=:★.12βδςρ∀ΠιλΠ≃"

}
