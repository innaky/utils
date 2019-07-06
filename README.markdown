# Utils

Generic utilities.

* utils:random-string number

## Usage and installation

```lisp
(ql:quickload "utils")
```

# API

## List utilities

* pg-reverse `lst' (function):
  Reverse the elements of the list. (Thanks Mr. Paul Graham)

```lisp
CL-USER> (pg-reverse '(1 2 3 4))
(4 3 2 1)
```

* list+ `lst number' (function):
  Adding `n' in a all elements of the list (lst is a list of numbers).

```lisp
CL-USER> (list+ '(1 2 3 4) 5)
(6 7 8 9)
```
* combine-cars `lst1 lst2' (function):
  Combine the elements of the list, first with first, second with second ...
  return a list of list.

```lisp
CL-USER> (combine-cars '(1 2 3) '(4 5 6))
((1 4) (2 5) (3 6))
```

* mklist `atom' (function):
  Transform an atom in a list

```lisp
CL-USER> (mklist 'abc)
(abc)
```

* firsts `lst' (function):
  `lst' is a list of lists. Extract the cars of the sublists.

```lisp
CL-USER> (first '((1 2 3) (4 5 6) (7 8 9)))
(1 4 7)
```

* my-member `atom lst' (function):
  This function check if `atom' exist in `lst'.

```lisp
CL-USER> (my-member 'abc '(1 3 4 abc add))
T
CL-USER> (my-member 'abc '(1 3 4 abcd add))
NIL
```

* rfember `atom lst' (function):
  Remove the first coincidence of `atom' in the `lst'.

```lisp
CL-USER> (rfember 'abc '(foo bar baz abc buu abc 1 2))
(FOO BAR BAZ BUU ABC 1 2)
```

* rember `atom lst' (function):
  Remove all coincidences of `atom' in the `lst'.

```lisp
CL-USER> (rember 'abc '(foo bar baz abc buu abc 1 2))
(FOO BAR BAZ BUU 1 2)
```

* replace-atom `new-atom old-atom lst' (function):
  Replace the first coincidence of old-atom to new-atom

```lisp
CL-USER> (replace-atom 'new 'old '(abc def 1 old 2 3 5 old))
(ABC DEF 1 NEW 2 3 5 OLD)
```

* full-replace-atom `new old lst' (function):
  Replace all the coincidences of the old atom for the new atom in the list.

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
