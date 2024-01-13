open Vitest
open Expect
open Ch3

test("computes product of an empty list", t => {
  t->assertions(1)

  expect(product(list{}))->toBe(1)
})

test("computes product of list with one item", t => {
  t->assertions(1)

  expect(product(list{1}))->toBe(1)
})

test("computes product of list with two items", t => {
  t->assertions(1)

  expect(product(list{1, 2}))->toBe(2)
})

test("computes product of list with three items", t => {
  t->assertions(1)

  expect(product(list{1, 2, 3})) ->toBe(6)
})
