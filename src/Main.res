let rec last = list => {
  switch list {
  | list{} => None
  | list{last} => Some(last)
  | list{_, ...rest} => last(rest)
  }
}

Js.log(last(list{}) === None)
Js.log(last(list{1}) === Some(1))
Js.log(last(list{1, 2}) === Some(2))
Js.log(last(list{1, 2, 3}) === Some(3))

let rec lastTwo = list => {
  switch list {
  | list{} | list{_} => None
  | list{x, y} => Some((x, y))
  | list{_, ...rest} => lastTwo(rest)
  }
}

Js.log(lastTwo(list{}) === None)
Js.log(lastTwo(list{1}) === None)
Js.log(lastTwo(list{1, 2}) == Some((1, 2)))
Js.log(lastTwo(list{1, 2, 3}) == Some((2, 3)))

let rec at = (list, n) => {
  switch (list, n) {
  | (list{}, _) => None
  | (list{first, ..._}, 0) => Some(first)
  | (list{_, ...rest}, _) => at(rest, n - 1)
  }
}

Js.log(at(list{}, 0) === None)
Js.log(at(list{1}, 0) === Some(1))
Js.log(at(list{1}, 1) === None)
Js.log(at(list{1, 2}, 0) === Some(1))
Js.log(at(list{1, 2}, 1) === Some(2))
Js.log(at(list{1, 2}, 2) === None)
Js.log(at(list{1, 2, 3}, 0) === Some(1))
Js.log(at(list{1, 2, 3}, 1) === Some(2))
Js.log(at(list{1, 2, 3}, 2) === Some(3))
Js.log(at(list{1, 2, 3}, 3) === None)

let rec length = (~count=0, list) => {
  switch list {
  | list{} => count
  | list{_, ...rest} => length(rest, ~count=count + 1)
  }
}

Js.log(length(list{}) === 0)
Js.log(length(list{1}) === 1)
Js.log(length(list{1, 2}) === 2)
Js.log(length(list{1, 2, 3}) === 3)

let rec reverse = (~acc=list{}, list) => {
  switch list {
  | list{} => acc
  | list{first, ...rest} => reverse(rest, ~acc=list{first, ...acc})
  }
}

Js.log(reverse(list{}) === list{})
Js.log(reverse(list{1}) == list{1})
Js.log(reverse(list{1, 2}) == list{2, 1})
Js.log(reverse(list{1, 2, 3}) == list{3, 2, 1})
