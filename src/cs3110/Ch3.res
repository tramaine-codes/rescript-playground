let rec product = lst => {
  switch lst {
  | list{} => 1
  | list{first, ...rest} => first * product(rest)
  }
}

let rec concat = lst => {
  switch lst {
  | list{} => ""
  | list{first, ...rest} => first ++ concat(rest)
  }
}

let patterns = lst => {
  switch lst {
  | list{"bigred", ..._} => true
  | list{_, _} | list{_, _, _, _} => true
  | list{x, y, ..._} => x === y
  | _ => false
  }
}

let fifth = lst => {
  List.nth_opt(lst, 5)
}

let sort_rev = List.sort((x, y) => y - x)
let sort_rev' = lst => {
  lst |> List.sort((x, y) => y - x)
}
let sort_rev'' = lst => {
  lst->List.sort((x, y) => y - x, _)
}
let sort_rev''' = lst => {
  lst->Belt.List.sort((x, y) => y - x)
}
let sort_rev'''' = lst => {
  lst |> Belt.List.sort(_, (x, y) => y - x)
}

let last = lst => {
  switch lst {
  | list{} => None
  | _ => List.nth_opt(lst, List.length(lst) - 1)
  }
}

let anyZeroes = {
  List.exists(x => x == 0)
}

let rec take = (n, lst) => {
  if n == 0 {
    list{}
  } else {
    switch lst {
    | list{} => lst
    | list{first, ...rest} => list{first, ...take(n - 1, rest)}
    }
  }
}

let rec take' = (n, ~acc=list{}, lst) => {
  if n == 0 {
    acc
  } else {
    switch lst {
    | list{} => acc
    | list{first, ...rest} => take'(n - 1, rest, ~acc=List.append(acc, list{first}))
    }
  }
}

let rec drop = (n, lst) => {
  if n == 0 {
    lst
  } else {
    switch lst {
    | list{} => lst
    | list{_, ...rest} => drop(n - 1, rest)
    }
  }
}

let rec print_list = lst => {
  switch lst {
  | list{} => ()
  | list{first, ...rest} =>
    Js.Console.log(first)
    print_list(rest)
  }
}

let print_list' = List.iter(Js.Console.log)

type student = {firstName: string, lastName: string, gpa: float}

let s = {firstName: "Tramaine", lastName: "Gillus", gpa: 3.7}
let fullName = student => (student.firstName, student.lastName)
let createStudent = (firstName, lastName, gpa) => {firstName, lastName, gpa}

type poketype = Normal | Fire | Water
type pokemon = {name: string, hp: int, ptype: poketype}

let charizard = {name: "charizard", hp: 79, ptype: Fire}
let ember = {name: "bubble", hp: 40, ptype: Fire}
let squirtle = {name: "squirtle", hp: 44, ptype: Water}
let bubble = {name: "bubble", hp: 40, ptype: Water}
let facade = {name: "facade", hp: 70, ptype: Normal}
let covet = {name: "covet", hp: 60, ptype: Normal}

let safeHd = lst => {
  switch lst {
  | list{} => None
  | list{first, ..._} => Some(first)
  }
}

let safeTl = lst => {
  switch lst {
  | list{} => None
  | list{_, ...rest} => Some(rest)
  }
}

Js.log(patterns(list{"bigred"}) == true)
Js.log(patterns(list{"bigred", "foo"}) == true)
Js.log(patterns(list{"foo"}) == false)
Js.log(patterns(list{"foo", "foo"}) == true)
Js.log(patterns(list{"foo", "foo", "bar"}) == true)
Js.log(patterns(list{"foo", "bar", "baz"}) == false)
Js.log(patterns(list{"foo", "bar", "baz", "qux"}) == true)

Js.log(sort_rev(list{}) == list{})
Js.log(sort_rev(list{1}) == list{1})
Js.log(sort_rev(list{1, 2}) == list{2, 1})
Js.log(sort_rev(list{1, 3, 2}) == list{3, 2, 1})

Js.log(sort_rev'(list{}) == list{})
Js.log(sort_rev'(list{1}) == list{1})
Js.log(sort_rev'(list{1, 2}) == list{2, 1})
Js.log(sort_rev'(list{1, 3, 2}) == list{3, 2, 1})

Js.log(sort_rev''(list{}) == list{})
Js.log(sort_rev''(list{1}) == list{1})
Js.log(sort_rev''(list{1, 2}) == list{2, 1})
Js.log(sort_rev''(list{1, 3, 2}) == list{3, 2, 1})

Js.log(sort_rev'''(list{}) == list{})
Js.log(sort_rev'''(list{1}) == list{1})
Js.log(sort_rev'''(list{1, 2}) == list{2, 1})
Js.log(sort_rev'''(list{1, 3, 2}) == list{3, 2, 1})

Js.log(sort_rev''''(list{}) == list{})
Js.log(sort_rev''''(list{1}) == list{1})
Js.log(sort_rev''''(list{1, 2}) == list{2, 1})
Js.log(sort_rev''''(list{1, 3, 2}) == list{3, 2, 1})

Js.log(last(list{}) == None)
Js.log(last(list{1}) == Some(1))
Js.log(last(list{1, 2}) == Some(2))
Js.log(last(list{1, 2, 3}) == Some(3))

Js.log(anyZeroes(list{}) == false)
Js.log(anyZeroes(list{1}) == false)
Js.log(anyZeroes(list{1, 2}) == false)
Js.log(anyZeroes(list{0, 1, 2}) == true)
Js.log(anyZeroes(list{1, 0, 2}) == true)
Js.log(anyZeroes(list{1, 2, 0}) == true)

Js.log(take(0, list{}) == list{})
Js.log(take(0, list{1}) == list{})
Js.log(take(1, list{1}) == list{1})
Js.log(take(2, list{1}) == list{1})
Js.log(take(1, list{1, 2}) == list{1})
Js.log(take(2, list{1, 2}) == list{1, 2})
Js.log(take(3, list{1, 2, 3}) == list{1, 2, 3})
Js.log(take(3, list{1, 2, 3, 4}) == list{1, 2, 3})

Js.log(take'(0, list{}) == list{})
Js.log(take'(0, list{1}) == list{})
Js.log(take'(1, list{1}) == list{1})
Js.log(take'(2, list{1}) == list{1})
Js.log(take'(1, list{1, 2}) == list{1})
Js.log(take'(2, list{1, 2}) == list{1, 2})
Js.log(take'(3, list{1, 2, 3}) == list{1, 2, 3})
Js.log(take'(3, list{1, 2, 3, 4}) == list{1, 2, 3})

Js.log(drop(0, list{}) == list{})
Js.log(drop(1, list{}) == list{})
Js.log(drop(0, list{1}) == list{1})
Js.log(drop(1, list{1}) == list{})
Js.log(drop(2, list{1}) == list{})
Js.log(drop(1, list{1, 2}) == list{2})
Js.log(drop(2, list{1, 2}) == list{})
Js.log(drop(2, list{1, 2, 3}) == list{3})
Js.log(drop(3, list{1, 2, 3}) == list{})
Js.log(drop(2, list{1, 2, 3, 4}) == list{3, 4})
Js.log(drop(3, list{1, 2, 3, 4}) == list{4})

print_list(list{})
print_list(list{1})
print_list(list{1, 2})
print_list(list{1, 2, 3})

print_list'(list{})
print_list'(list{1})
print_list'(list{1, 2})
print_list'(list{1, 2, 3})

Js.Console.log(createStudent("Jane", "Doe", 3.2) == {firstName: "Jane", lastName: "Doe", gpa: 3.2})

Js.Console.log(safeHd(list{}) == None)
Js.Console.log(safeHd(list{1}) == Some(1))
Js.Console.log(safeHd(list{1, 2}) == Some(1))
Js.Console.log(safeHd(list{1, 2, 3}) == Some(1))

Js.Console.log(safeTl(list{}) == None)
Js.Console.log(safeTl(list{1}) == Some(list{}))
Js.Console.log(safeTl(list{1, 2}) == Some(list{2}))
Js.Console.log(safeTl(list{1, 2, 3}) == Some(list{2, 3}))
