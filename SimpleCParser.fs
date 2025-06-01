//
// SimpleC parser.  This component checks the input program against
// SimpleC syntax and returns "Success!" for legal source or a
// diagnostic beginning with "syntax_error:" otherwise.
//
// Zarak Khan

namespace compiler

module parser =
  //-----------------------------------------------------------------
  // helper utilities
  //-----------------------------------------------------------------
  /// Returns true if *literal* starts with *pattern*.
  let private beginswith (pattern:string) (literal:string) =
    literal.StartsWith pattern

  /// Binary operator tokens recognised by <expr-op>.
  let private operators =
    ["+";"-";"*";"/";"^";"<";"<=";">";">=";"==";"!="]

  //-----------------------------------------------------------------
  // token matcher
  //-----------------------------------------------------------------
  /// Consumes *expected_token* from the head of *tokens*.
  /// Throws a descriptive exception if the token does not match.
  let private matchToken expected_token tokens =
    let next_token = List.head tokens
    if expected_token = next_token then List.tail tokens
    else failwith ("expecting " + expected_token + ", but found " + next_token)

  //-----------------------------------------------------------------
  // grammar implementation (one function per non‑terminal)
  //-----------------------------------------------------------------
  /// Parses <simpleC> → "void main() { <stmts> } $".
  let rec private simpleC tokens =
    let t1 = matchToken "void" tokens
    let t2 = matchToken "main" t1
    let t3 = matchToken "(" t2
    let t4 = matchToken ")" t3
    let t5 = matchToken "{" t4
    let t6 = stmts t5
    let t7 = matchToken "}" t6
    matchToken "$" t7   // reach EOF token

  /// Parses <stmts> → <stmt> <morestmts>.
  and private stmts tokens =
    let t1 = stmt tokens
    morestmts t1

  /// Parses <morestmts> → <stmt> <morestmts> | EMPTY.
  and private morestmts tokens =
    match tokens with
    | "}"::_ -> tokens   // EMPTY
    | _       -> morestmts (stmt tokens)

  /// Dispatches to the correct concrete statement based on look‑ahead.
  and private stmt tokens =
    match tokens with
    | ";"::_                -> empty_stmt tokens
    | "int"::_              -> vardecl tokens
    | "cin"::_              -> input tokens
    | "cout"::_             -> output tokens
    | "if"::_               -> ifstmt tokens
    | tok::_ when beginswith "identifier:" tok -> assignment tokens
    | tok::_ -> failwith ("expecting statement, but found " + tok)
    | [] -> failwith "unexpected end of file during parsing statement"

  //-----------------------------------------------------------------
  // statement variants
  //-----------------------------------------------------------------
  and private empty_stmt tokens = matchToken ";" tokens

  and private vardecl tokens =
    let t1 = matchToken "int" tokens
    let idtok = List.head t1
    if beginswith "identifier:" idtok then
      let t2 = List.tail t1
      matchToken ";" t2
    else
      failwith ("expecting identifier, but found " + idtok)

  and private input tokens =
    let t1 = matchToken "cin" tokens
    let t2 = matchToken ">>" t1
    let idtok = List.head t2
    if beginswith "identifier:" idtok then
      let t3 = List.tail t2
      matchToken ";" t3
    else
      failwith ("expecting identifier, but found " + idtok)

  and private output tokens =
    let t1 = matchToken "cout" tokens
    let t2 = matchToken "<<" t1
    let t3 = output_value t2
    matchToken ";" t3

  and private output_value tokens =
    match tokens with
    | "endl"::rest -> rest
    | _             -> expr_value tokens

  and private assignment tokens =
    let idtok = List.head tokens
    if beginswith "identifier:" idtok then
      let t1 = List.tail tokens
      let t2 = matchToken "=" t1
      let t3 = expr t2
      matchToken ";" t3
    else
      failwith ("expecting identifier, but found " + idtok)

  and private ifstmt tokens =
    let t1 = matchToken "if" tokens
    let t2 = matchToken "(" t1
    let t3 = expr t2
    let t4 = matchToken ")" t3
    let t5 = stmt t4            // then‑branch
    else_part t5                // optional else‑branch

  and private else_part tokens =
    match tokens with
    | "else"::_ -> stmt (matchToken "else" tokens)
    | _          -> tokens

  //-----------------------------------------------------------------
  // expressions
  //-----------------------------------------------------------------
  and private expr tokens =
    let t1 = expr_value tokens
    match t1 with
    | op::_ when List.contains op operators ->
        let t2 = expr_op t1
        expr_value t2
    | _ -> t1

  and private expr_value tokens =
    let next = List.head tokens
    let rest = List.tail tokens
    if beginswith "identifier:" next ||
       beginswith "int_literal:" next ||
       beginswith "str_literal:" next ||
       next = "true" || next = "false" then
      rest
    else
      failwith ("expecting identifier or literal, but found " + next)

  and private expr_op tokens =
    let op = List.head tokens
    if List.contains op operators then
      List.tail tokens
    else
      failwith ("expecting expression operator, but found " + op)

  //-----------------------------------------------------------------
  // public entry
  //-----------------------------------------------------------------
  let parse tokens =
    try
      let _ = simpleC tokens
      "Success!"
    with ex -> "syntax_error: " + ex.Message
