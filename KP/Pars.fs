module ParserModule
open FParsec

let single_quoted_string : Parser<Value, Unit> = 
    pchar '\"' >>. manyCharsTill anyChar (pchar '\"')
    |>> string |>> Str
    .>> spaces

let bool_parser: Parser<Value, Unit> =
    pword "true" <|> pword "false"
    |>> function
        | "true" -> Bool (true)
        | "false" -> Bool (false)

let int_parser : Parser<Value, Unit> =
    pint32 |>> Int

let value_parser : Parser<Value, Unit> =
    choice [
        attempt int_parser
        attempt single_quoted_string
        attempt bool_parser
    ]

let input_string_parser s = pstring s .>> spaces
let parenthesized_expr_parser s = between (input_string_parser "(") (input_string_parser ")") s

let literal_parser: Parser<Expr, Unit> = value_parser |>> Literal

let variable_parser: Parser<Expr, Unit> =
    many1Satisfy2 (System.Char.IsLetter) (System.Char.IsLetterOrDigit) |>> Variable .>> spaces

let operator_precedence_parser = OperatorPrecedenceParser<Expr, Unit, Unit>()

let expr_parser = operator_precedence_parser.ExpressionParser

let term_parser = choice [
    int_parser .>> spaces |>> Literal <|> variable_parser
    parenthesized_expr_parser expr_parser
]

operator_precedence_parser.TermParser <- term_parser

let create_operation op x y = Operation (x, op, y)

type OperatorProperties = {Symbol: string;
                           Precedence: int;
                           Operator: Operator }

let int_operators = [
    {Symbol = ">"; Precedence = 1; Operator = Gt}
    {Symbol = "<"; Precedence = 1; Operator = Lt}
    {Symbol = ">="; Precedence = 1; Operator = Gte}
    {Symbol = "<="; Precedence = 1; Operator = Lte}
    {Symbol = "=="; Precedence = 1; Operator = Eq}
    {Symbol = "!="; Precedence = 1; Operator = Neq}
    {Symbol = "+"; Precedence = 2; Operator = Add}
    {Symbol = "-"; Precedence = 2; Operator = Sub}
    {Symbol = "*"; Precedence = 3; Operator = Mult}
    {Symbol = "/"; Precedence = 3; Operator = Div}
    {Symbol = "%"; Precedence = 3; Operator = Mod}
]

let add_operators (parser: OperatorPrecedenceParser<Expr, Unit, Unit>) operator_table =
    operator_table
    |> List.iter (fun details ->
        let operator  =
          InfixOperator(
              details.Symbol,
              spaces,
              details.Precedence,
              Associativity.Left,
              create_operation details.Operator
          )
        parser.AddOperator(operator))

add_operators operator_precedence_parser int_operators


let string_parser = OperatorPrecedenceParser<Expr, Unit, Unit>()
let string_expr_parser = string_parser.ExpressionParser

let string_term_parser = choice [
    literal_parser
    variable_parser
    expr_parser
    parenthesized_expr_parser string_expr_parser 
]
string_parser.TermParser <- string_term_parser

let string_operators = [
    {Symbol = "++"; Precedence = 1; Operator = Sconcat}
]

add_operators string_parser string_operators

let bool_operator_parser = OperatorPrecedenceParser<Expr, Unit, Unit>()
let bool_expr_parser = bool_operator_parser.ExpressionParser
let bool_term_parser = choice [
    bool_parser .>> spaces |>> Literal
    variable_parser
    parenthesized_expr_parser bool_expr_parser
]
bool_operator_parser.TermParser <- bool_term_parser

let bool_operators = [
    {Symbol = "and"; Precedence = 2; Operator =  And}
    {Symbol = "or"; Precedence = 1; Operator = Or}
]

add_operators bool_operator_parser bool_operators

let operation_parser = choice [
    attempt expr_parser
    attempt string_expr_parser
    attempt bool_expr_parser
]

let expression_parser = choice [
    operation_parser
    literal_parser
    variable_parser
]

let if_statement_parser: Parser<Statement, Unit> =
    let condition_parser = input_string_parser "if" >>. expression_parser
    let inner_block_parser = block_parser
    let else_block_parser = input_string_parser "else" >>. block_parser |> opt
    
    pipe3 condition_parser inner_block_parser else_block_parser (fun condition inner else_block -> If (condition, inner, else_block))

let while_statement_parser: Parser<Statement, Unit> =
    let condition_parser = input_string_parser "while" >>. expression_parser
    condition_parser .>>. block_parser
    |>> While

do statement_parser_ref := choice [
    print_parser
    if_statement_parser
    while_statement_parser
    set_parser
]
