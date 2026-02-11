; Modified from upstream tree-sitter-haskell/queries/highlights.scm
(variable) @variable

(pattern/wildcard) @variable

(decl/function
  patterns: (patterns
    (_) @variable.parameter))

(expression/lambda
  (_)+ @variable.parameter
  "->")

(decl/function
  (infix
    (pattern) @variable.parameter))

(integer) @number

(negation) @number

(float) @number.float

(char) @character

(string) @string

(comment) @comment

((haddock) @comment.documentation)

[
  "("
  ")"
  "{"
  "}"
  "["
  "]"
] @punctuation.bracket

[
  ","
  ";"
] @punctuation.delimiter

[
  "forall"
  "∀"
] @keyword.repeat

(pragma) @keyword.directive

[
  "if"
  "then"
  "else"
  "case"
  "of"
] @keyword.conditional

(lambda_cases "cases" @keyword.conditional)

(import
  [
    "import"
    "qualified"
    "as"
    "hiding"
  ] @keyword.import)

[
  (operator)
  (constructor_operator)
  (all_names)
  (wildcard)
  "."
  ".."
  "="
  "|"
  "::"
  "=>"
  "->"
  "<-"
  "\\"
  "`"
  "@"
] @operator

(import_package) @string

(module_id) @module

[ "module" ] @keyword.module

[
  "where"
  "let"
  "in"
  "class"
  "instance"
  "pattern"
  "data"
  "newtype"
  "type"
  "deriving"
  "do"
  "mdo"
  "infix"
  "infixl"
  "infixr"
  "default"
] @keyword

(deriving
  (deriving_strategy) @keyword)

(rec "rec" @keyword)

(data_family "family" @keyword)

(type_family "family" @keyword)

(name) @type

(type/star) @type

(constructor) @constructor

((constructor) @boolean
  (#any-of? @boolean "True" "False"))

(comment) @spell
