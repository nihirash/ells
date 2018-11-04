# ELLS Standard forms description

## MathForms library

This forms located at `com.nihirash.ells.lib.MathForms` class.

### +, -, *, /

This forms performs simple math operations. Accepts any count of numeric arguments.

Example:

```
(+ 1 (- 10 1)
     (* 3 3 3)
     (/ 10 2))
```

### mod

This binary form returns module of number.

Example:

```
(mod 3 2)
```

### min(max)

This form accepts one numeric list and returns minimal(maximum) element.

Example:

```
(list (min '(33 22 12))
      (max '(22 11 12)))
```

### > and <

This binary forms compares first argument and seconds and returns boolean value as result.

Example:

```
(and (< 1 20) (> 20 1))
```

### =

This binary form check values to equality(not only numeric forms).

Example:

```
(= 1 (- 2 1))
```