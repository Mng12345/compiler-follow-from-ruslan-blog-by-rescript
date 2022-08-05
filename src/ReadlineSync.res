type t

@module external readline: t = "readline-sync"

@send external question_: (t, string) => string = "question"

let question = preStr => {
  question_(readline, preStr)
}
