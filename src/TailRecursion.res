// Tail recursion explanation:
//   https://stackoverflow.com/questions/33923/what-is-tail-recursion
//

let rec add1 = list => {
  switch list {
  | list{} => 0
  | list{n, ...rest} => n + add1(rest)
  }
}

let rec add2 = (~total=0, list) => {
  switch list {
  | list{} => total
  | list{n, ...rest} => add2(rest, ~total=n + total)
  }
}

let myList = list{1, 2, 3}

Js.log(add1(myList))
Js.log(add2(myList))
