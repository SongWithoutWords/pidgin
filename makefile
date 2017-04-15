
# I think this is a good workflow... I think...

dirs := mk/dir

build := mk/build

parse-test := mk/parse-test
lex-test := mk/lex-test

ast-src := src/Ast.hs
parser-src := src/Parser.y
parser-gen := gen/Parser.hs
parser-inf := info/Parser.info

tkn-src := src/Tokens.hs
lexer-src := src/Lexer.x
lexer-gen := gen/Lexer.hs

test-cases := test/TestCases.hs

test: $(build) $(lex-test) $(parse-test)

parser: $(parser-gen)

run: build
	@stack exec creole-exe

$(build): $(lexer-gen) $(parser-gen) src
	@stack build
	@touch $(build)

$(parse-test): $(parser-gen) $(ast-src) $(test-cases)
	@echo "Testing parser"
	@touch $(parse-test)
	@stack test --test-arguments '-p parser'

$(lex-test): $(lexer-gen) $(tkn-src) $(test-cases)
	@echo "Testing lexer"
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

