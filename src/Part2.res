module Token = {
  exception NotToken
  module Operator = {
    type t = Plus | Subtract
  }
  type t = Integer(int) | Operator(Operator.t) | Eof | Whitespace
}

module Interpreter = {
  exception WrongExpression

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
        | Token.Eof => 
          Token.Eof
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
    let left = tokens->Belt.List.get(0)
    let operator = tokens->Belt.List.get(1)
    let right = tokens->Belt.List.get(2)
    let end = tokens->Belt.List.get(3)
    switch (left, operator, right, end) {
    | (
        Some(Token.Integer(left)),
        Some(Token.Operator(operator)),
        Some(Token.Integer(right)),
        Some(Token.Eof),
      ) =>
      switch operator {
      | Token.Operator.Plus => left + right
      | Token.Operator.Subtract => left - right
      }
    | _ => raise(WrongExpression)
    }
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
