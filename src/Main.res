let rec last = (list) => {
  switch list {
  | list{} => None
  | list{last} => Some(last)
  | list{_, ...rest} => last(rest)
  }
}

let rec lastTwo = (list) => {
  switch list {
  | list{} | list{_} => None
  | list{x, y} => Some((x, y))
  | list{_, ...rest} => lastTwo(rest)
  }
}

Js.log(last(list{}));
Js.log(last(list{1}));
Js.log(last(list{1, 2}));
Js.log(last(list{1, 2, 3}));
Js.log(lastTwo(list{}));
Js.log(lastTwo(list{1}));
Js.log(lastTwo(list{1, 2}));
Js.log(lastTwo(list{1, 2, 3}));
