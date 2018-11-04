# ELLS Standard forms description

## ListForms library

This forms located at `com.nihirash.ells.lib.ListForms` class.

### list

This forms creates list with values from evaluated arguments of function

Example:

```
(list 1 2 3 "hello!")
```

### head

This form returns first element of list or nil if list empty.

Example:

```
(head (list 1 2 3))
```

### tail

This form returns all list except first element or nil if list contains only one element.

Example:

```
(tail '(1 2 3))
```

### list-append

This form appends element to list and returns it as new list(operation doesn't destructive).

Example:

```
(list-append '(1 2 3) 4 5)
```

### merge-lists

This form merges two lists onto one list and returns it as new list.

Example:

```
(merge-lists '(1 2 3) '(4 5))
```