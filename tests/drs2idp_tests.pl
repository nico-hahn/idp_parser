:- begin_tests(drs2idpTests).

test(func2idp_filter) :-
  assertion(
    filterValues([], [])
  ),
  assertion(
    filterValues(
      [
        f_value(func(a), b),
        f_value(func(b), c),
        pred(a, b),
        f_value(func(c), d),
        pred(a, c),
        f_value(func(d), e)
      ],
      [
        f_value(func(a), b),
        f_value(func(b), c),
        f_value(func(c), d),
        f_value(func(d), e)
      ]
    )
  ).

test(func2idp_sortValues) :-
  assertion(
    sortValues([], [])
  ),
  assertion(
    sortValues(
      [
        f_value(f1(a1), b1),
        f_value(f2(a2), b2),
        f_value(f2(c2), d2),
        f_value(f1(c1), d1),
        f_value(f1(e1), f1)
      ],
      [
        f1([
          f1([a1], b1),
          f1([c1], d1),
          f1([e1], f1)
        ]),
        f2([
          f2([a2], b2),
          f2([c2], d2)
        ])
      ]
    )
  ).

test(fund2idp_findAll) :-
  assertion(
    findAll(a, [], [], [])
  ),
  assertion(
    findAll(
      f1,
      [
        f_value(f1(a1), b1),
        f_value(f1(c1), d1),
        f_value(f2(a2), b2),
        f_value(f2(c2), d2),
        f_value(f1(e1), f1)
      ],
      [
        f_value(f2(a2), b2),
        f_value(f2(c2), d2)
      ],
      [
        f1([a1], b1),
        f1([c1], d1),
        f1([e1], f1)
      ]
    )
  ).

:- end_tests(drs2idpTests).