cabal:=cabal new-

build: parser
	${cabal}build

test: parser
	${cabal}run test

lexer: Parser/Token.x
	alex Parser/Token.x

parser: Parser/Parse.y lexer
	happy -a -g Parser/Parse.y

repl-core:
	${cabal}repl core
