open Test
open Ch3

test("computes product of an empty list", () => {
  assertion((x, y) => x === y, product(list{}), 1)
})

test("computes product of list with one item", () => {
  assertion((x, y) => x === y, product(list{1}), 1)
})

test("computes product of list with two items", () => {
  assertion((x, y) => x === y, product(list{1, 2}), 2)
})

test("computes product of list with three items", () => {
  assertion((x, y) => x === y, product(list{1, 2, 3}), 6)
})
