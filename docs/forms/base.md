# ELLS Standard forms description

## BaseForms library

This forms located at `com.nihirash.ells.lib.BaseForms` class.

### quote

`quote` is a form that's preserve code from evaluation. For evaluate some code inside quote you may use `unquote` form.

Example:

```
(quote (1 2 3 (unquote (+ 3 1))))
```

`quote` and `unquote` also have syntax equvalent:

```
'(1 2 3 @(+ 3 1))
```

### eval

`eval` is a form that's performs evaluation code given as argument to it. This may be used for some meta-programming.

Example:

```
(def x 'y)
(def y 0)

(eval '(set @x 1))
y
```

### do

`do` is form that's make possible combine code-block into one expression. Usable for conditions.

Example:

```
(def x 1)

(if (= x 1) (do
               (set x 2)
               x)
            (+ x 2))
```

### if

`if` is default condition form.

It gets 3 or 2 arguments (if **condition** **true-branch** **false-branch**). False-branch can be absent - nil will be returned in false condition.

### and

`and` is form that's evaluates all arguments until gets false-like value. And returns boolean true if all expression was evaluated and returns not false values else returns false.

Example:

```
(and 1 2 3)
```

### or

`or` is form that's evaluates all arguments until gets any non false-like value. It returns true if one of evaluated expressions was true-like else returns false.

Example:

```
(def not-evaled true)

(or nil false true not-evaled)
```

### not

`not` is unary form that's casts value to boolean and reverts it.

### def

`def` is defines definition in current lexical scope. Scope are common for values and functions.

Example:

```
(def x 1)
(def y (+ x 2))
```

### set

`set` is changes value of definition. It tries found definition in current lexical scope(if it doesn't declared on this level it will be looked on upper level, recursively)

Example:

```
(def x 1)

(defn setx (value)
    (set x value))

(setx 10)
x
```

### fn

`fn` is form that created function(like "lambda" in classic lisps). First argument is list of parameters, rest is a function body.

Example:

```
(map (fn (e) (+ e 1)) '(1 2 3))
```

**NB** map is not default form, but present in standard **pre-defined environment** as definition.

## defn

`defn` is syntax-sugar on def and fn - that's makes definition for functions.

Example:

```
(defn fun (val)
    (* val 3))

(fun 3)
```

### is-nil?, is-number?, is-string?, is-boolean?, is-fun?, is-list?

This unary forms checks value for specified type and returns true only if arguments type equal to specified.

Example:

```
(and
  (is-nil? ())
  (is-number? 666)
  (is-string? "yes")
  (is-fun? map)
  (is-list? '(1 2 3)))
```

### to-string

This unary form converts anything to string and returns it.

Example:

```
(to-string '(1 2 3 @(string-append "hello " "world")))
```

### to-number

This unary form tries convert argument to numeric data-type. If it isn't possible will be thrown exception.

Example:

```
(to-number "123.66")
```

### to-long

This unary form tries convert argument to numberic data-type and converts it to long-integer type.  If it isn't possible will be thrown exception.

Example:

```
(to-long "123.66")
```


### throw

This form throws EllsRuntimeException with some specified text(from scala-side it's looks like Left[String] with message).

Example:

```
(throw "Oh no")
```

### try

This form is binary - first argument is expression(or do-block), second catch-expression(block that's contains `exception` definition with exception message).

Example:

```
(try
 (do
   (throw "My exception")
   1)
 (= exception "My exception"))
```

