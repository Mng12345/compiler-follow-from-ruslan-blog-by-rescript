module Token = {
  exception NotToken
  type t = Integer(int) | Plus | Eof
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

  let getNextToken = interpreter => {
    if interpreter.pos > interpreter.text->Js.String2.length - 1 {
      Token.Eof
    } else {
      let char = interpreter.text->Js.String2.charAt(interpreter.pos)
      switch char {
      | "+" =>
        interpreter.pos = interpreter.pos + 1
        Token.Plus
      | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" =>
        interpreter.pos = interpreter.pos + 1
        char->Belt.Int.fromString->Belt.Option.getUnsafe->Token.Integer
      | _ => raise(Token.NotToken)
      }
    }
  }

  let expr = interpreter => {
    let left = interpreter->getNextToken
    let plus = interpreter->getNextToken
    let right = interpreter->getNextToken
    let end = interpreter->getNextToken
    switch (left, plus, right, end) {
    | (Token.Integer(left), Token.Plus, Token.Integer(right), Token.Eof) => left + right
    | _ => raise(WrongExpression)
    }
  }
}

let main = () => {
  while true {
    try {
      let text = ReadlineSync.question("calc>")
      let integer = Interpreter.make(text)
      let value = integer->Interpreter.expr
      Js.log(value)
    } catch {
    | exn => Js.log2(`error: `, exn)
    }
  }
}

main()
