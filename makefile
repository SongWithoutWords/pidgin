
# I think this is a good workflow... I think...

dirs := mk/dir

build := mk/build


type-check-test := mk/typecheck-test
type-errors-src := src/TypeErrors.hs
type-check-src := src/TypeCheck.hs

parse-test := mk/parse-test
ast-src := src/Ast.hs
parser-src := src/Parser.y
parser-gen := gen/Parser.hs
parser-inf := info/Parser.info

lex-test := mk/lex-test
tkn-src := src/Tokens.hs
lexer-src := src/Lexer.x
lexer-gen := gen/Lexer.hs

test-cases := test/TestCases.hs
test-main := test/Test.hs

build-and-test: $(build) $(lex-test) $(parse-test) $(type-check-test)

parser: $(parser-gen)

run: build
	@stack exec pidgin-exe

$(build): $(lexer-gen) $(parser-gen) src
	@stack build
	@touch $(build)

$(type-check-test): $(type-check-src) $(type-errors-src) $(test-cases) $(test-main)
	@touch $(type-check-test)
	@stack test --test-arguments '-p "type inference"'
	@stack test --test-arguments '-p "type errors"'

$(parse-test): $(parser-gen) $(ast-src) $(test-cases) $(test-main)
	@touch $(parse-test)
	@stack test --test-arguments '-p parser'

$(lex-test): $(lexer-gen) $(tkn-src) $(test-cases) $(test-main)
	@touch $(lex-test)
	@stack test --test-arguments '-p lexer'

$(parser-gen): $(dirs) $(parser-src)
	@echo "Generating parser"
	@happy -o $(parser-gen) -i$(parser-inf) $(parser-src)

$(lexer-gen): $(dirs) $(lexer-src)
	@echo "Generating lexer"
	@alex $(lexer-src) -o gen/Lexer.hs

$(dirs):
	@echo "Creating directories"
	@mkdir -p gen
	@mkdir -p info
	@mkdir -p mk
	@touch $(dirs)

