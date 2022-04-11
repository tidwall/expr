# expr

[![GoDoc](https://godoc.org/github.com/tidwall/expr?status.svg)](https://godoc.org/github.com/tidwall/expr)

Expression evaluator for Go

## Features

- Operators: `+` `-` `*` `/` `%` `!` `<` `<=` `>` `>=` `==` `===` `!=` `!===` `?:` `??` `,` `[]` `&` `|` `^`
- Types: String, Number, Boolean, and custom types
- Supports custom functions
- Parenthesized expressions
- Javascript-like syntax with automatic type conversions
- Native uint64 and int64 types using the `u64` and `i64` suffix on number literals

## Using

To start using `expr`, install Go and run `go get`:

```sh
$ go get github.com/tidwall/expr
```

### Basic expressions

For example:

```js
1 + 1
(10 * 5 <= 50) && (50 > 100 || 8 >= 7)
1e+10 > 0 ? "big" : "small"
```

In Go, you're code may look like the following.

```go
res, _ := expr.Eval(`1 + 1`, nil)
fmt.Println(res)
res, _ := expr.Eval(`(10 * 5 <= 50) && (50 > 100 || 8 >= 7)`, nil)
fmt.Println(res)
res, _ := expr.Eval(`1e+10 > 0 ? "big" : "small"`, nil)
fmt.Println(res)

// Output: 
// 2
// true
// big
```

### Advanced expressions 

Using a custom evaluation extender we can extend the eval function to support 
arithmetic and comparisons on custom types, such as `time.Time` that is built into Go.
We can also provide some extra user data that exposes extra variables to the evaluator.

Example expressions:

```js
timestamp
timestamp - dur('1h')
now() + dur('24h')
timestamp < now() - dur('24h') ? "old" : "new"
((minX + maxX) / 2) + "," + ((minY + maxY) / 2)
```

In Go, you would provide a custom `Extender` to the `Eval` function.

```go
package main

import (
	"fmt"
	"time"

	"github.com/tidwall/expr"
)

func main() {
	// Create a user data map that can be referenced by the Eval function.
	dict := make(map[string]expr.Value)

	// Add a bounding box to the user dictionary.
	dict["minX"] = expr.Number(112.8192)
	dict["minY"] = expr.Number(33.4738)
	dict["maxX"] = expr.Number(113.9146)
	dict["maxY"] = expr.Number(34.3367)

	// Add a timestamp value to the user dictionary.
	ts, _ := time.Parse(time.RFC3339, "2022-03-31T09:00:00Z")
	dict["timestamp"] = expr.Object(ts)

	// Set up an evaluation extender for referencing the user data, and
	// using functions and operators on custom types.
	ext := expr.NewExtender(
		func(info expr.RefInfo, ctx *expr.Context) (expr.Value, error) {
			if info.Chain {
				// Only use globals in this example.
				// No chained objects like `user.name`.
				return expr.Undefined, nil
			}
			switch info.Ident {
			case "now":
				// The `now()` function
				return expr.Function("now"), nil
			case "dur":
				// The `dur(str)` function
				return expr.Function("duration"), nil
			default:
				// Check the user dictionary.
				umap, ok := ctx.UserData.(map[string]expr.Value)
				if !ok {
					// value not found in
					return expr.Undefined, nil
				}
				return umap[info.Ident], nil
			}
		},
		func(info expr.CallInfo, ctx *expr.Context) (expr.Value, error) {
			if info.Chain {
				// Only use globals in this example.
				// No chained function like `user.name()`.
				return expr.Undefined, nil
			}
			switch info.Ident {
			case "now":
				// Return the current date/time.
				return expr.Object(time.Now()), nil
			case "dur":
				// Compute the arguments.
				args, err := info.Args.Compute()
				if err != nil {
					return expr.Undefined, err
				}
				// Parse the duration using the first argument.
				d, err := time.ParseDuration(args.Get(0).String())
				if err != nil {
					return expr.Undefined, err
				}
				// Valid time.Duration, return as an Int64 value
				return expr.Int64(int64(d)), nil
			default:
				return expr.Undefined, nil
			}
		},
		func(info expr.OpInfo, ctx *expr.Context) (expr.Value, error) {
			// Try to convert a and/or b to time.Time
			left, leftOK := info.Left.Value().(time.Time)
			right, rightOK := info.Right.Value().(time.Time)
			if leftOK && rightOK {
				// Both values are time.Time.
				// Perform comparison operation.
				switch info.Op {
				case expr.OpLt:
					return expr.Bool(left.Before(right)), nil
				case expr.OpLte:
					return expr.Bool(!right.After(left)), nil
				case expr.OpGt:
					return expr.Bool(left.After(right)), nil
				case expr.OpGte:
					return expr.Bool(!left.Before(right)), nil
				}
			} else if leftOK || rightOK {
				// Either A or B are time.Time.
				// Perform arithmatic add/sub operation and return a
				// recalcuated time.Time value.
				var x time.Time
				var y int64
				if leftOK {
					x = left
					y = info.Right.Int64()
				} else {
					x = right
					y = info.Left.Int64()
				}
				switch info.Op {
				case expr.OpAdd:
					return expr.Object(x.Add(time.Duration(y))), nil
				case expr.OpSub:
					return expr.Object(x.Add(-time.Duration(y))), nil
				}
			}
			return expr.Undefined, nil
		},
	)

	// Set up a custom expr.context that holds user data and the extender.
	ctx := expr.Context{UserData: dict, Extender: ext}

	var res expr.Value

	// Return the timestamp.
	res, _ = expr.Eval(`timestamp`, &ctx)
	fmt.Println(res)

	// Subtract an hour from the timestamp.
	res, _ = expr.Eval(`timestamp - dur('1h')`, &ctx)
	fmt.Println(res)

	// Add one day to the current time.
	res, _ = expr.Eval(`now() + dur('24h')`, &ctx)
	fmt.Println(res)

	// See if timestamp is older than a day
	res, _ = expr.Eval(`timestamp < now() - dur('24h') ? "old" : "new"`, &ctx)
	fmt.Println(res)

	// Get the center of the bounding box as a concatenated string.
	res, _ = expr.Eval(`((minX + maxX) / 2) + "," + ((minY + maxY) / 2)`, &ctx)
	fmt.Println(res)

	// Output:
	// 2022-03-31 09:00:00 +0000 UTC
	// 2022-03-31 08:00:00 +0000 UTC
	// 2022-04-02 06:00:40.834656 -0700 MST m=+86400.000714835
	// old
	// 113.36689999999999,33.905249999999995
}
```
