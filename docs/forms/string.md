# ELLS Standard forms description

## StringForms library

This forms located at `com.nihirash.ells.lib.StringForms` class.

### string-length

This forms returns string length.

Example:

```
(string-length "Hello")
```

### substring

This forms creates new string from substring of first argument by skipping **second-argument** symbols and with **third-argument** length.

Example:

```
(substring "-=()=-" 2 2)
```

### string-append

This form concatenate all arguments to new string.

Example:

```
(string-append
 "Hello, " "world" "!")
```

### split-string

This form splits string(first argument) with using separator(second argument).

Example:

```
(split-string "1,2,3" ",")
```