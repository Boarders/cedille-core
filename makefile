cabal:=cabal new-

build: parser
	${cabal}build

lexer: Parser/Token.x
	alex Parser/Token.x

parser: Parser/Parse.y lexer
	happy Parser/Parse.y

repl-core:
	${cabal}repl core

