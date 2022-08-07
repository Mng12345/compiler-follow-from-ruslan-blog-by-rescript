// The same code with Part2.res, it already made a calculator for arithmetic operators in the expression
module Token = {
  exception NotToken
  module Operator = {
    type t = Plus | Subtract | Multiply | Divide
  }
  type t = Integer(int) | Operator(Operator.t) | Eof | Whitespace
}

module TokenNode = {
  type t = {
    mutable value: Token.t,
    mutable left: option<Token.t>,
    mutable right: option<Token.t>,
  }
}

module Interpreter = {
  exception WrongExpression
  exception NeverHappenedInFilteredTokens
  exception NoTokens

  type t = {
    text: string,
    mutable pos: int,
    mutable currentToken: Token.t,
  }

  let make = text => {
    let pos = 0
    let currentToken = Token.Eof
    {
      text: text,
      pos: pos,
      currentToken: currentToken,
    }
  }

  let getNextSingleToken = interpreter => {
    if interpreter.pos > interpreter.text->Js.String2.length - 1 {
      interpreter.pos = interpreter.pos + 1
      Token.Eof
    } else {
      let char = interpreter.text->Js.String2.charAt(interpreter.pos)
      switch char {
      | "+" =>
        interpreter.pos = interpreter.pos + 1
        Token.Operator(Token.Operator.Plus)
      | "-" =>
        interpreter.pos = interpreter.pos + 1
        Token.Operator(Token.Operator.Subtract)
      | "*" =>
        interpreter.pos = interpreter.pos + 1
        Token.Operator(Token.Operator.Multiply)
      | "/" =>
        interpreter.pos = interpreter.pos + 1
        Token.Operator(Token.Operator.Divide)
      | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" =>
        interpreter.pos = interpreter.pos + 1
        char->Belt.Int.fromString->Belt.Option.getUnsafe->Token.Integer
      | " " =>
        interpreter.pos = interpreter.pos + 1
        Token.Whitespace
      | _ => raise(Token.NotToken)
      }
    }
  }

  let getNextToken = interpreter => {
    let rec getNextToken = (interpreter, left) => {
      let right = interpreter->getNextSingleToken
      switch (left, right) {
      | (None, _) =>
        switch right {
        | Token.Eof => Token.Eof
        | _ => getNextToken(interpreter, right->Some)
        }
      | (Some(left), _) =>
        switch (left, right) {
        | (Token.Integer(left), Token.Integer(right)) =>
          getNextToken(interpreter, Token.Integer(left * 10 + right)->Some)
        | (Token.Eof, _) => Token.Eof
        | (Token.Integer(_), _)
        | (Token.Whitespace, _)
        | (Token.Operator(_), _) =>
          interpreter.pos = interpreter.pos - 1
          left
        }
      }
    }
    getNextToken(interpreter, None)
  }

  let extractTokens = interpreter => {
    let rec extractTokens_ = (interpreter, list) => {
      let token = interpreter->getNextToken
      switch token {
      | Token.Whitespace => extractTokens_(interpreter, list)
      | Token.Eof => list->Belt.List.add(token)
      | _ =>
        let list = list->Belt.List.add(token)
        extractTokens_(interpreter, list)
      }
    }
    extractTokens_(interpreter, list{})->Belt.List.reverse
  }

  let execute = tokens => {
    let rec makeNode = (rootNode: TokenNode.t, tokens) => {
      switch (tokens, rootNode.value) {
      | (list{}, _) => rootNode
      | (list{h, ...t}, _) =>
        switch (rootNode.value, h) {
        | (Token.Integer(_), Token.Eof) => rootNode
        | (Token.Integer(_), Token.Operator(_)) =>
          {
            value: h,
            left: rootNode.value->Some,
            right: None,
          }->makeNode(t)
        | (Token.Operator(operator), Token.Integer(right)) =>
          switch rootNode.left {
          | None => raise(NeverHappenedInFilteredTokens)
          | Some(left) =>
            switch left {
            | Token.Integer(left) =>
              switch operator {
              | Token.Operator.Divide => {
                  value: Token.Integer(left / right),
                  left: None,
                  right: None,
                }
              | Token.Operator.Multiply => {
                  value: Token.Integer(left * right),
                  left: None,
                  right: None,
                }
              | Token.Operator.Plus => {
                  value: Token.Integer(left + right),
                  left: None,
                  right: None,
                }
              | Token.Operator.Subtract => {
                  value: Token.Integer(left - right),
                  left: None,
                  right: None,
                }
              }->makeNode(t)
            | _ => raise(NeverHappenedInFilteredTokens)
            }
          }
        | (Token.Operator(_), _) => raise(NeverHappenedInFilteredTokens)
        | (Token.Whitespace, _) => raise(NeverHappenedInFilteredTokens)
        | (Token.Integer(_), Token.Integer(_)) => raise(NeverHappenedInFilteredTokens)
        | (Token.Integer(_), Token.Whitespace) => raise(NeverHappenedInFilteredTokens)
        | (Token.Eof, _) => raise(NeverHappenedInFilteredTokens)
        }
      }
    }
    switch tokens {
    | list{h, ...t} =>
      switch makeNode({value: h, left: None, right: None}, t).value {
      | Token.Integer(v) => v
      | _ => raise(NeverHappenedInFilteredTokens)
      }
    | list{} => raise(NoTokens)
    }
  }

  let expr = interpreter => {
    let tokens =
      interpreter
      ->extractTokens
      ->Belt.List.keep(token =>
        switch token {
        | Token.Whitespace => false
        | _ => true
        }
      )
    execute(tokens)
  }
}

let main = () => {
  while true {
    try {
      let text = ReadlineSync.question("calc>")
      let interpreter = Interpreter.make(text)
      let value = interpreter->Interpreter.expr
      Js.log(value)
    } catch {
    | exn => Js.log2(`error: `, exn)
    }
  }
}

main()
