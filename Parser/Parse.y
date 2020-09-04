{
module Parser.Parse 
  ( parseModule 
  , parseDef
  , parseTerm
  ) where

import Parser.Token
import Parser.Util
import qualified Parser.AST as AST

}


%name parseModule Module
%name parseDef    Def
%name parseTerm   Term
%tokentype { Token }

%monad { Parser } { thenP } { returnP }
%lexer { lexer  } { Token EOF }

%token
    module        { Token Module             }
    enddef        { Token EndDef             }
    name          { Token  (Name $$)         }
    '('           { Token LeftRoundBracket   }
    ')'           { Token RightRoundBracket  }
    '['           { Token LeftSquareBracket  }
    ']'           { Token RightSquareBracket }
    '{'           { Token LeftCurlyBracket   }
    '}'           { Token RightCurlyBracket  }
     '@'          { Token At                 }
     ','          { Token Comma              }
     '-'          { Token Dash               }
     '='          { Token Equals             }
     ann          { Token Ann                }
     kind         { Token Kind               }
     box          { Token Box                }
     dot          { Token Dot                }
     one          { Token One                }
     two          { Token Two                }
     beta         { Token Beta               }
     indabsurd    { Token IndAbsurd          }
     symm         { Token Symm               }
     rewrite      { Token Rewrite            }
     phi          { Token Phi                }
     forall       { Token ForAll             }
     pi           { Token Pi                 }
     intersection { Token Intersection       }
     lam          { Token Lam                }
     bigLam       { Token BigLam             }
     eq           { Token Eq                 }
%%

Module :: { AST.Module }
        : module name dot Defs { AST.Module $2 $4 }

Defs :: { AST.Defs }
            : Def { [$1] }
            | Def enddef { [$1] }
            | Def enddef Defs { $1 : $3 }


Def :: { AST.Def }
     : name '=' Term { AST.Def $1 $3 }

Term :: { AST.Term }
      : ConstrTerm { $1 }

ConstrTerm :: { AST.Term }
           : Atom dot one { AST.Pr1 $1 }
           | Atom dot two { AST.Pr2 $1 }
           | beta Atom '{' Term '}' { AST.Beta $2 $4 }
           | indabsurd Atom Atom { AST.IndAbsurd $2 $3 }
           | symm Atom { AST.Symm $2 }
           | rewrite Atom '@' name dot Atom '-' Atom { AST.ReWrite $2 $4 $6 $8 }
           | forall name ann Atom dot ConstrTerm { AST.ForAll $2 $4 $6 }
           | pi name ann Atom dot ConstrTerm { AST.Pi $2 $4 $6 }
           | intersection name ann Atom dot Atom { AST.Iota $2 $4 $6 }
           | lam name ann Atom dot ConstrTerm { AST.Lam $2 $4 $6 }
           | bigLam name ann Atom dot ConstrTerm { AST.LamEr $2 $4 $6 }
           | '[' ConstrTerm ',' ConstrTerm '@' name dot Atom ']' { AST.Both $2 $4 $6 $8 }
           | phi Atom '-' Atom '{' ConstrTerm '}' { AST.Phi $2 $4 $6 }
           | '[' name '=' ConstrTerm '@' ConstrTerm ']' '-' ConstrTerm { AST.Let $2 $4 $6 $9 }
           | Atom eq Atom { AST.Eq $1 $3 }
           | AppSpine { $1 }


AppSpine :: { AST.Term }
          : AppSpine Atom { AST.App $1 $2 }
          | AppSpine '-' Atom { AST.AppEr $1 $3 }
          | Atom { $1 }

Atom :: { AST.Term }
      : '(' Term ')' { $2 }
      | name         { AST.Var $1 }
      | box          { AST.Box  }
      | kind         { AST.Star }
