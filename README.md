# expr

[![GoDoc](https://godoc.org/github.com/tidwall/expr?status.svg)](https://godoc.org/github.com/tidwall/expr)

Expression evaluator for Go

## Features

- Operators: `+ - * / % ! < <= == != > >=`
- Types: String, Number, Boolean, and Custom types
- Parenthesized expressions
- Javascript-like syntax and automatic type conversions

## Using

To start using `expr`, install Go and run `go get`:

```sh
$ go get github.com/tidwall/expr
```

### Simple expressions

```go
res, _ := expr.Eval("1 + 1", nil)
fmt.Println(res)
res, _ := expr.Eval("(10 * 5 <= 50) && (50 > 100 || 8 >= 7)", nil)
fmt.Println(res)

// Output: 
// 2
// true
```

### Advanced expressions using a custom evaluation extender.

Let's extend the eval function to support arithmetic and comparisons on
the `time.Time` type. And, we'll also provide some extra user data that exposes
extra variables to the evaluator.

```go
// Create a user data map that can be referenced by the Eval function.
umap := make(map[string]expr.Value)

// Add a bounding box to the user data map.
umap["minX"] = expr.Number(112.8192)
umap["minY"] = expr.Number(33.4738)
umap["maxX"] = expr.Number(113.9146)
umap["maxY"] = expr.Number(34.3367)

// Add a timestamp value to the user data map.
ts, _ := time.Parse(time.RFC3339, "2022-03-31T09:00:00Z")
umap["timestamp"] = expr.Custom(ts)

// Set up an evaluation extender for referencing the user data and
// using operators on custom types.
ext := expr.NewExtender(
  func(data string, udata any) (expr.Value, error) {
    switch data {
    case "now":
      // Get the seconds since Epoch.
      return expr.Custom(time.Now()), nil
    default:
      if len(data) >= 2 && data[0] == '{' && data[len(data)-1] == '}' {
        // Try parsing a time.Duration.
        s := data[1 : len(data)-1]
        d, err := time.ParseDuration(s)
        if err != nil {
          return expr.Undefined, err
        }
        // Valid time.Duration, return as an Int64 value
        return expr.Int64(int64(d)), nil
      }
      // Not a time.Duration, check the umap for the data
      umap := udata.(map[string]expr.Value)
      return umap[data], nil
    }
  },
  func(op expr.Op, a, b expr.Value, udata any) (expr.Value, error) {
    // Try to convert a and/or b to time.Time
    at, aok := a.Value().(time.Time)
    bt, bok := b.Value().(time.Time)
    if aok && bok {
      // Both values are time.Time.
      // Perform comparison operation.
      switch op {
      case expr.OpLt:
        return expr.Bool(at.Before(bt)), nil
      case expr.OpLte:
        return expr.Bool(!bt.After(at)), nil
      case expr.OpGt:
        return expr.Bool(at.After(bt)), nil
      case expr.OpGte:
        return expr.Bool(!at.Before(bt)), nil
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
      case expr.OpAdd:
        return expr.Custom(x.Add(time.Duration(y))), nil
      case expr.OpSub:
        return expr.Custom(x.Add(-time.Duration(y))), nil
      }
    }
    return expr.Undefined, nil
  },
)

// Set up the options
opts := expr.Options{UserData: umap, Extender: ext}

var res expr.Value

// Return the timestamp.
res, _ = expr.Eval("timestamp", &opts)
fmt.Println(res)

// Subtract an hour from the timestamp.
res, _ = expr.Eval("timestamp - {1h}", &opts)
fmt.Println(res)

// Add one day to the current time.
res, _ = expr.Eval("now + {24h}", &opts)
fmt.Println(res)

// Compare current time to the timestamp.
res, _ = expr.Eval("now > timestamp", &opts)
fmt.Println(res)

// Get the center of the bounding box as a concatenated string.
res, _ = expr.Eval(`((minX + maxX) / 2) + "," + ((minY + maxY) / 2)`, &opts)
fmt.Println(res)

// Output:
// 2022-03-31 09:00:00 +0000 UTC
// 2022-03-31 08:00:00 +0000 UTC
// 2022-04-01 12:09:38.313927 -0700 MST m=+86400.001490167
// true
// 113.36689999999999,33.905249999999995
```
