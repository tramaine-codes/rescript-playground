let rec last = list => {
  switch list {
  | list{} => None
  | list{last} => Some(last)
  | list{_, ...rest} => last(rest)
  }
}

Console.log(last(list{}) === None)
Console.log(last(list{1}) === Some(1))
Console.log(last(list{1, 2}) === Some(2))
Console.log(last(list{1, 2, 3}) === Some(3))

let rec lastTwo = list => {
  switch list {
  | list{} | list{_} => None
  | list{x, y} => Some((x, y))
  | list{_, ...rest} => lastTwo(rest)
  }
}

Console.log(lastTwo(list{}) === None)
Console.log(lastTwo(list{1}) === None)
Console.log(lastTwo(list{1, 2}) == Some((1, 2)))
Console.log(lastTwo(list{1, 2, 3}) == Some((2, 3)))

let rec at = (list, n) => {
  switch (list, n) {
  | (list{}, _) => None
  | (list{first, ..._}, 0) => Some(first)
  | (list{_, ...rest}, _) => at(rest, n - 1)
  }
}

Console.log(at(list{}, 0) === None)
Console.log(at(list{1}, 0) === Some(1))
Console.log(at(list{1}, 1) === None)
Console.log(at(list{1, 2}, 0) === Some(1))
Console.log(at(list{1, 2}, 1) === Some(2))
Console.log(at(list{1, 2}, 2) === None)
Console.log(at(list{1, 2, 3}, 0) === Some(1))
Console.log(at(list{1, 2, 3}, 1) === Some(2))
Console.log(at(list{1, 2, 3}, 2) === Some(3))
Console.log(at(list{1, 2, 3}, 3) === None)

let rec length = (~count=0, list) => {
  switch list {
  | list{} => count
  | list{_, ...rest} => length(rest, ~count=count + 1)
  }
}

Console.log(length(list{}) === 0)
Console.log(length(list{1}) === 1)
Console.log(length(list{1, 2}) === 2)
Console.log(length(list{1, 2, 3}) === 3)

let rec reverse = (~acc=list{}, list) => {
  switch list {
  | list{} => acc
  | list{first, ...rest} => reverse(rest, ~acc=list{first, ...acc})
  }
}

Console.log(reverse(list{}) === list{})
Console.log(reverse(list{1}) == list{1})
Console.log(reverse(list{1, 2}) == list{2, 1})
Console.log(reverse(list{1, 2, 3}) == list{3, 2, 1})
