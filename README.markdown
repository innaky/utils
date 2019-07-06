# Utils

Generic utilities.

* utils:random-string number

## Usage and installation

```lisp
(ql:quickload "utils")
```

# API

## List utilities

* (**pg-reverse** _lst_)    _function_:
  Reverse the elements of the list. (Thanks Mr. Paul Graham)

```lisp
CL-USER> (pg-reverse '(1 2 3 4))
(4 3 2 1)
```

* (**list+** _lst number_)    _function_:
  Adding _number_ in a all elements of the _lst_ (_lst_ is a list of numbers).

```lisp
CL-USER> (list+ '(1 2 3 4) 5)
(6 7 8 9)
```
* (**combine-cars** _lst1 lst2_)    _function_:
  Combine the elements of the list, first with first, second with second ...
  return a list of list.

```lisp
CL-USER> (combine-cars '(1 2 3) '(4 5 6))
((1 4) (2 5) (3 6))
```

* (**mklist** _atom_)     _function_:
  Transform an atom in a list

```lisp
CL-USER> (mklist 'abc)
(abc)
```

* (**firsts** _lst_)     _function_:
  _lst_ is a list of lists. Extract the cars of the sublists.

```lisp
CL-USER> (first '((1 2 3) (4 5 6) (7 8 9)))
(1 4 7)
```

* (**my-member** _atom lst_)     _function_:
  Check if _atom_ exist in _lst_.

```lisp
CL-USER> (my-member 'abc '(1 3 4 abc add))
T
CL-USER> (my-member 'abc '(1 3 4 abcd add))
NIL
```

* (**rfember** _atom lst_)     _function_:
  Remove the first coincidence of _atom_ in the _lst_.

```lisp
CL-USER> (rfember 'abc '(foo bar baz abc buu abc 1 2))
(FOO BAR BAZ BUU ABC 1 2)
```

* (**rember** _atom lst_)     _function_:
  Remove all coincidences of _atom_ in the _lst_.

```lisp
CL-USER> (rember 'abc '(foo bar baz abc buu abc 1 2))
(FOO BAR BAZ BUU 1 2)
```

* (**replace-atom** _new-atom old-atom lst_)     _function_:
  Replace the first coincidence of _old-atom_ to _new-atom_

```lisp
CL-USER> (replace-atom 'new 'old '(abc def 1 old 2 3 5 old))
(ABC DEF 1 NEW 2 3 5 OLD)
```

* (**full-replace-atom** _new old lst_)     _function_:
  Replace all the coincidences of the _old_ atom for the _new_ atom in the list.

```lisp
CL-USER> (full-replace-atom 'new 'old '(abc def 1 old 2 3 5 old))
(ABC DEF 1 NEW 2 3 5 NEW)
```
## Author

* Innaky (innaky@protonmail.com)

## Copyright

Copyright (c) 2019 Innaky (innaky@protonmail.com)

## License

Licensed under the GPLv3 License.
