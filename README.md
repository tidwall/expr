# expr

[![GoDoc](https://godoc.org/github.com/tidwall/expr?status.svg)](https://godoc.org/github.com/tidwall/expr)

Expression evaluator for Go

## Features

- Operators: `+` `-` `*` `/` `%` `!` `<` `<=` `==` `!=` `>` `>=` `?:` `??` `,`
- Types: String, Number, Boolean, and Custom types
- Parenthesized expressions
- Javascript-like syntax and automatic type conversions

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
timestamp - $1h
now + $24h
timestamp < now - $24h ? "old" : "new"
((minX + maxX) / 2) + "," + ((minY + maxY) / 2)
```

In Go, you would provide a custom `Extender` to the `Eval` function.

```go
// Create a user data map that can be referenced by the Eval function.
umap := make(map[string]Value)

// Add a bounding box to the user data map.
umap["minX"] = Number(112.8192)
umap["minY"] = Number(33.4738)
umap["maxX"] = Number(113.9146)
umap["maxY"] = Number(34.3367)

// Add a timestamp value to the user data map.
ts, _ := time.Parse(time.RFC3339, "2022-03-31T09:00:00Z")
umap["timestamp"] = Object(ts)

// Set up an evaluation extender for referencing the user data and
// using operators on custom types.
ext := NewExtender(
	func(expr string, ctx *Context) (Value, error) {
		switch expr {
		case "now":
			// Get the seconds since Epoch.
			return Object(time.Now()), nil
		default:
			if len(expr) >= 1 && expr[0] == '$' {
				// Try parsing a time.Duration.
				s := expr[1:]
				d, err := time.ParseDuration(s)
				if err != nil {
					return Undefined, err
				}
				// Valid time.Duration, return as an Int64 value
				return Int64(int64(d)), nil
			}
			// Not a time.Duration, check the umap for the data
			umap, ok := ctx.UserData.(map[string]Value)
			if !ok {
				return Undefined, ErrUndefined
			}
			return umap[expr], nil
		}
	},
	func(op Op, a, b Value, ctx *Context) (Value, error) {
		// Try to convert a and/or b to time.Time
		at, aok := a.Value().(time.Time)
		bt, bok := b.Value().(time.Time)
		if aok && bok {
			// Both values are time.Time.
			// Perform comparison operation.
			switch op {
			case OpLt:
				return Bool(at.Before(bt)), nil
			case OpLte:
				return Bool(!bt.After(at)), nil
			case OpGt:
				return Bool(at.After(bt)), nil
			case OpGte:
				return Bool(!at.Before(bt)), nil
			}
		} else if aok || bok {
			// Either A or B are time.Time.
			// Perform arithmatic add/sub operation and return a
			// recalcuated time.Time value.
			var x time.Time
			var y int64
			if aok {
				x = at
				y = b.Int64()
			} else {
				x = bt
				y = a.Int64()
			}
			switch op {
			case OpAdd:
				return Object(x.Add(time.Duration(y))), nil
			case OpSub:
				return Object(x.Add(-time.Duration(y))), nil
			}
		}
		return Undefined, ErrUndefined
	},
)

// Set up a custom context that holds user data and the extender.
ctx := Context{UserData: umap, Extender: ext}

var res Value

// Return the timestamp.
res, _ = Eval(`timestamp`, &ctx)
fmt.Println(res)

// Subtract an hour from the timestamp.
res, _ = Eval(`timestamp - $1h`, &ctx)
fmt.Println(res)

// Add one day to the current time.
res, _ = Eval(`now + $24h`, &ctx)
fmt.Println(res)

// See if timestamp is older than a day
res, _ = Eval(`timestamp < now - $24h ? "old" : "new"`, &ctx)
fmt.Println(res)

// Get the center of the bounding box as a concatenated string.
res, _ = Eval(`((minX + maxX) / 2) + "," + ((minY + maxY) / 2)`, &ctx)
fmt.Println(res)

// Output:
// 2022-03-31 09:00:00 +0000 UTC
// 2022-03-31 08:00:00 +0000 UTC
// 2022-04-02 06:00:40.834656 -0700 MST m=+86400.000714835
// old
// 113.36689999999999,33.905249999999995
```
