// Copyright 2022 Joshua J Baker. All rights reserved.
// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file.

package expr

import (
	"errors"
	"fmt"
	"math"
	"strconv"
	"strings"
	"unicode/utf16"
	"unicode/utf8"
)

func errUndefined(ident string, pos int, chain bool) error {
	var err error
	if chain {
		err = fmt.Errorf("Uncaught TypeError: "+
			"Cannot read properties of undefined (reading '%s')", ident)
	} else {
		err = fmt.Errorf("ReferenceError: %s is not defined", ident)
	}
	return &errEval{pos: pos, err: err, udef: true}
}

func errOperator(err error, pos int) error {
	return &errEval{
		pos: pos, err: fmt.Errorf("OperatorError: %w", err),
	}
}

func errReference(err error, pos int) error {
	return &errEval{
		pos: pos, err: fmt.Errorf("ReferenceError: %w", err),
	}
}

func errCall(err error, pos int) error {
	return &errEval{
		pos: pos, err: fmt.Errorf("CallError: %w", err),
	}
}

func errSyntax(pos int) error {
	return &errEval{
		pos: pos, err: errors.New("SyntaxError"),
	}
}

// ErrStop is used to stop the EvalForEach and ForEachValue
var ErrStop = errors.New("stop")

type int64er interface{ Int64() int64 }
type uint64er interface{ Uint64() uint64 }
type float64er interface{ Float64() float64 }
type booler interface{ Bool() bool }
type stringer interface{ String() string }

type errEval struct {
	pos  int
	err  error
	udef bool
}

func (err *errEval) Error() string {
	return err.err.Error()
}

// CharPosOfErr returns the character position of where the error occured in
// the Eval function, or -1 if unknown
func CharPosOfErr(err error) int {
	if err, ok := err.(*errEval); ok {
		return err.pos
	}
	return -1
}

var (
	Undefined = Value{kind: undefKind}
	Null      = Value{kind: nullKind}
)

// Op is an operator for Custom values used for the Options.Op function.
type Op string

const (
	OpAdd  Op = "+"
	OpSub  Op = "-"
	OpMul  Op = "*"
	OpDiv  Op = "/"
	OpMod  Op = "%"
	OpLt   Op = "<"
	OpSeq  Op = "==="
	OpAnd  Op = "&&"
	OpOr   Op = "||"
	OpCoal Op = "??"
)

func (op Op) String() string {
	return string(op)
}

type kind byte

const (
	undefKind kind = iota // undefined
	nullKind              // null
	boolKind              // bool
	floatKind             // float64
	intKind               // int64
	uintKind              // uint64
	strKind               // string
	funcKind              // function
	objKind               // custom object
)

// Value represents is the return value of Eval.
type Value struct {
	kind     kind        // kind
	boolVal  bool        // bool
	floatVal float64     // float64
	intVal   int64       // int64
	uintVal  uint64      // uint64
	strVal   string      // string (and function name)
	objVal   interface{} // custom object
}

// String returns a string value.
func String(s string) Value { return Value{kind: strKind, strVal: s} }

// Number returns a float64 value.
func Number(x float64) Value { return Float64(x) }

// Bool returns a bool value.
func Bool(t bool) Value { return Value{kind: boolKind, boolVal: t} }

// Float64 returns an int64 value.
func Float64(x float64) Value { return Value{kind: floatKind, floatVal: x} }

// Int64 returns an int64 value.
func Int64(x int64) Value { return Value{kind: intKind, intVal: x} }

// Uint64 returns a uint64 value.
func Uint64(x uint64) Value { return Value{kind: uintKind, uintVal: x} }

// Object returns a custom user-defined object.
func Object(v interface{}) Value {
	return Value{kind: objKind, objVal: v}
}

func (a Value) TypeOf() string {
	switch a.kind {
	case undefKind:
		return "undefined"
	case boolKind:
		return "boolean"
	case floatKind, intKind, uintKind:
		return "number"
	case strKind:
		return "string"
	case funcKind:
		return "function"
	default:
		return "object"
	}
}

// Function
func Function(name string) Value { return Value{kind: funcKind, strVal: name} }

func doOp(op Op, a, b Value, pos int, ctx *Context) (Value, error) {
	if ctx != nil && ctx.Extender != nil {
		info := OpInfo{Left: a, Op: op, Right: b}
		v, err := ctx.Extender.Op(info, ctx)
		if err == nil {
			return v, nil
		}
		return Undefined, errOperator(err, pos)
	}
	return Undefined, errOperator(errors.New("undefined "), pos)
}

func (a Value) add(b Value, pos int, ctx *Context) (Value, error) {
	if a.kind == objKind || b.kind == objKind {
		return doOp(OpAdd, a, b, pos, ctx)
	}
	if a.kind == b.kind {
		switch a.kind {
		case floatKind:
			return Value{kind: floatKind, floatVal: a.floatVal + b.floatVal}, nil
		case intKind:
			return Value{kind: intKind, intVal: a.intVal + b.intVal}, nil
		case uintKind:
			return Value{kind: uintKind, uintVal: a.uintVal + b.uintVal}, nil
		case strKind:
			return Value{kind: strKind, strVal: a.strVal + b.strVal}, nil
		case boolKind, undefKind, nullKind:
			a, b = a.tofval(), b.tofval()
			return Value{kind: floatKind, floatVal: a.floatVal + b.floatVal}, nil
		}
	} else if a.isnum() && b.isnum() {
		a, b = a.tofval(), b.tofval()
		return Value{kind: floatKind, floatVal: a.floatVal + b.floatVal}, nil
	}
	a, b = a.tostr(), b.tostr()
	return Value{kind: strKind, strVal: a.strVal + b.strVal}, nil
}

func (a Value) isnum() bool {
	switch a.kind {
	case floatKind, intKind, uintKind, boolKind, nullKind, undefKind:
		return true
	}
	return false
}

func (a Value) sub(b Value, pos int, ctx *Context) (Value, error) {
	if a.kind == objKind || b.kind == objKind {
		return doOp(OpSub, a, b, pos, ctx)
	}
	if a.kind == b.kind {
		switch a.kind {
		case floatKind:
			return Value{kind: floatKind, floatVal: a.floatVal - b.floatVal}, nil
		case intKind:
			return Value{kind: intKind, intVal: a.intVal - b.intVal}, nil
		case uintKind:
			return Value{kind: uintKind, uintVal: a.uintVal - b.uintVal}, nil
		}
	}
	a, b = a.tofval(), b.tofval()
	return Value{kind: floatKind, floatVal: a.floatVal - b.floatVal}, nil
}

func (a Value) mul(b Value, pos int, ctx *Context) (Value, error) {
	if a.kind == objKind || b.kind == objKind {
		return doOp(OpMul, a, b, pos, ctx)
	}
	if a.kind == b.kind {
		switch a.kind {
		case floatKind:
			return Value{kind: floatKind, floatVal: a.floatVal * b.floatVal}, nil
		case intKind:
			return Value{kind: intKind, intVal: a.intVal * b.intVal}, nil
		case uintKind:
			return Value{kind: uintKind, uintVal: a.uintVal * b.uintVal}, nil
		}
	}
	a, b = a.tofval(), b.tofval()
	return Value{kind: floatKind, floatVal: a.floatVal * b.floatVal}, nil
}

func (a Value) div(b Value, pos int, ctx *Context) (Value, error) {
	if a.kind == objKind || b.kind == objKind {
		return doOp(OpDiv, a, b, pos, ctx)
	}
	if a.kind == b.kind {
		switch a.kind {
		case floatKind:
			return Value{kind: floatKind, floatVal: a.floatVal / b.floatVal}, nil
		case intKind:
			if b.intVal == 0 {
				return Value{kind: floatKind, floatVal: math.NaN()}, nil
			}
			return Value{kind: intKind, intVal: a.intVal / b.intVal}, nil
		case uintKind:
			if b.uintVal == 0 {
				return Value{kind: floatKind, floatVal: math.NaN()}, nil
			}
			return Value{kind: uintKind, uintVal: a.uintVal / b.uintVal}, nil
		}
	}
	a, b = a.tofval(), b.tofval()
	return Value{kind: floatKind, floatVal: a.floatVal / b.floatVal}, nil
}
func (a Value) mod(b Value, pos int, ctx *Context) (Value, error) {
	if a.kind == objKind || b.kind == objKind {
		return doOp(OpMod, a, b, pos, ctx)
	}
	if a.kind == b.kind {
		switch a.kind {
		case floatKind:
			return Value{kind: floatKind, floatVal: math.Mod(a.floatVal, b.floatVal)}, nil
		case intKind:
			if b.intVal == 0 {
				return Value{kind: floatKind, floatVal: math.NaN()}, nil
			}
			return Value{kind: intKind, intVal: a.intVal % b.intVal}, nil
		case uintKind:
			if b.uintVal == 0 {
				return Value{kind: floatKind, floatVal: math.NaN()}, nil
			}
			return Value{kind: uintKind, uintVal: a.uintVal % b.uintVal}, nil
		}
	}
	a, b = a.tofval(), b.tofval()
	return Value{kind: floatKind, floatVal: math.Mod(a.floatVal, b.floatVal)}, nil
}

func (a Value) lt(b Value, pos int, ctx *Context) (Value, error) {
	if a.kind == objKind || b.kind == objKind {
		return doOp(OpLt, a, b, pos, ctx)
	}
	if a.kind == b.kind {
		switch a.kind {
		case floatKind:
			return Value{kind: boolKind, boolVal: a.floatVal < b.floatVal}, nil
		case intKind:
			return Value{kind: boolKind, boolVal: a.intVal < b.intVal}, nil
		case uintKind:
			return Value{kind: boolKind, boolVal: a.uintVal < b.uintVal}, nil
		case strKind:
			return Value{kind: boolKind, boolVal: a.strVal < b.strVal}, nil
		}
	}
	a, b = a.tofval(), b.tofval()
	return Value{kind: boolKind, boolVal: a.floatVal < b.floatVal}, nil
}

func (a Value) lte(b Value, pos int, ctx *Context) (Value, error) {
	t, err := a.lt(b, pos, ctx)
	if err != nil {
		return Undefined, err
	}
	if t.Bool() {
		return t, nil
	}
	t, err = b.lt(a, pos, ctx)
	if err != nil {
		return Undefined, err
	}
	return Bool(!t.Bool()), nil
}

func (a Value) gt(b Value, pos int, ctx *Context) (Value, error) {
	return b.lt(a, pos, ctx)
}

func (a Value) gte(b Value, pos int, ctx *Context) (Value, error) {
	t, err := a.gt(b, pos, ctx)
	if err != nil {
		return Undefined, err
	}
	if t.Bool() {
		return t, nil
	}
	t, err = b.gt(a, pos, ctx)
	if err != nil {
		return Undefined, err
	}
	return Bool(!t.Bool()), nil
}

func (a Value) eq(b Value, pos int, ctx *Context) (Value, error) {
	t, err := a.lt(b, pos, ctx)
	if err != nil {
		return Undefined, err
	}
	if t.Bool() {
		return Bool(false), nil
	}
	t, err = b.lt(a, pos, ctx)
	if err != nil {
		return Undefined, err
	}
	return Bool(!t.Bool()), nil
}

func (a Value) seq(b Value, pos int, ctx *Context) (Value, error) {
	if a.kind == b.kind {
		return a.eq(b, pos, ctx)
	}
	return Value{kind: boolKind, boolVal: false}, nil
}

func (a Value) neq(b Value, pos int, ctx *Context) (Value, error) {
	val, err := a.eq(b, pos, ctx)
	if err != nil {
		return Undefined, err
	}
	return Bool(!val.Bool()), nil
}

func (a Value) sneq(b Value, pos int, ctx *Context) (Value, error) {
	val, err := a.seq(b, pos, ctx)
	if err != nil {
		return Undefined, err
	}
	return Bool(!val.Bool()), nil
}

func (a Value) and(b Value, pos int, ctx *Context) (Value, error) {
	if a.kind == objKind || b.kind == objKind {
		return doOp(OpAnd, a, b, pos, ctx)
	}
	a, b = a.tobool(), b.tobool()
	return Value{kind: boolKind, boolVal: a.boolVal && b.boolVal}, nil
}

func (a Value) or(b Value, pos int, ctx *Context) (Value, error) {
	if a.kind == objKind || b.kind == objKind {
		return doOp(OpOr, a, b, pos, ctx)
	}
	a, b = a.tobool(), b.tobool()
	return Value{kind: boolKind, boolVal: a.boolVal || b.boolVal}, nil
}

func (a Value) coalesce(b Value, pos int, ctx *Context) (Value, error) {
	if a.kind == objKind || b.kind == objKind {
		return doOp(OpCoal, a, b, pos, ctx)
	}
	switch a.kind {
	case undefKind, nullKind:
		return b, nil
	}
	return a, nil
}

func (a Value) tostr() Value {
	var s string
	switch a.kind {
	case nullKind:
		s = "null"
	case boolKind:
		s = strconv.FormatBool(a.boolVal)
	case floatKind:
		if math.IsInf(a.floatVal, 0) {
			if a.floatVal < 0 {
				s = "-Infinity"
			} else {
				s = "Infinity"
			}
		} else {
			s = strconv.FormatFloat(a.floatVal, 'f', -1, 64)
		}
	case intKind:
		s = strconv.FormatInt(a.intVal, 10)
	case uintKind:
		s = strconv.FormatUint(a.uintVal, 10)
	case strKind:
		s = a.strVal
	case funcKind:
		s = "[Function " + a.strVal + "]"
	case objKind:
		if v, ok := a.objVal.(stringer); ok {
			s = v.String()
		}
		s = fmt.Sprint(a.objVal)
	default:
		s = "undefined"
	}
	return Value{kind: strKind, strVal: s}
}

func (a Value) tofval() Value {
	switch a.kind {
	case nullKind:
		return Value{kind: floatKind, floatVal: 0}
	case boolKind:
		if a.boolVal {
			return Value{kind: floatKind, floatVal: 1}
		}
		return Value{kind: floatKind, floatVal: 0}
	case floatKind:
		return a
	case intKind:
		return Value{kind: floatKind, floatVal: float64(a.intVal)}
	case uintKind:
		return Value{kind: floatKind, floatVal: float64(a.uintVal)}
	case strKind:
		x, err := strconv.ParseFloat(a.strVal, 64)
		if err != nil {
			break
		}
		return Value{kind: floatKind, floatVal: x}
	case objKind:
		if v, ok := a.objVal.(float64er); ok {
			return Value{kind: floatKind, floatVal: v.Float64()}
		}
		return a.tostr().tofval()
	}
	return Value{kind: floatKind, floatVal: math.NaN()}
}

func (a Value) tobool() Value {
	if a.kind == boolKind {
		return a
	}
	var t bool
	switch a.kind {
	case strKind:
		t = a.strVal != ""
	case objKind:
		if v, ok := a.objVal.(booler); ok {
			t = v.Bool()
		} else {
			t = a.tofval().floatVal != 0
		}
	case undefKind, nullKind:
		t = false
	default:
		t = a.tofval().floatVal != 0
	}
	return Value{kind: boolKind, boolVal: t}
}

// Bool returns a boolean representation.
func (a Value) Bool() bool {
	return a.tobool().boolVal
}

// String returns a string representation.
func (a Value) String() string {
	return a.tostr().strVal
}

// Number returns s float64 representation.
func (a Value) Number() float64 {
	return a.Float64()
}

// Float64 returns s float64 representation.
func (a Value) Float64() float64 {
	return a.tofval().floatVal
}

// Int64 returns an int64 representation.
func (a Value) Int64() int64 {
	switch a.kind {
	case intKind:
		return a.intVal
	case uintKind:
		return int64(a.uintVal)
	case objKind:
		if v, ok := a.objVal.(int64er); ok {
			return v.Int64()
		}
	}
	return int64(a.tofval().floatVal)
}

// Uint64 returns a uint64 representation.
func (a Value) Uint64() uint64 {
	switch a.kind {
	case intKind:
		return uint64(a.intVal)
	case uintKind:
		return a.uintVal
	case objKind:
		if v, ok := a.objVal.(uint64er); ok {
			return v.Uint64()
		}
	}
	return uint64(a.tofval().floatVal)
}

// Value returns the native Go representation, which is one of the following:
//
//    bool, int64, uint64, float64, string, or nil (if undefined)
//
func (a Value) Value() interface{} {
	switch a.kind {
	case objKind:
		return a.objVal
	case boolKind:
		return a.boolVal
	case floatKind:
		return a.floatVal
	case intKind:
		return a.intVal
	case uintKind:
		return a.uintVal
	case strKind:
		return a.strVal
	default:
		return nil
	}
}

func closech(open byte) byte {
	switch open {
	case '(':
		return ')'
	case '[':
		return ']'
	case '{':
		return '}'
	}
	return open
}

func evalAtom(expr string, pos, steps int, ctx *Context) (Value, error) {
	expr, pos = trim(expr, pos)
	if len(expr) == 0 {
		return Undefined, errSyntax(pos)
	}

	var left Value
	var leftReady bool

	// first look for non-chainable atoms
	switch expr[0] {
	case '0':
		if len(expr) > 1 && (expr[1] == 'x' || expr[1] == 'X') {
			// hexadecimal
			x, err := strconv.ParseUint(expr[2:], 16, 64)
			if err != nil {
				return Undefined, errSyntax(pos)
			}
			return Float64(float64(x)), nil
		}
		fallthrough
	case '-', '.', '1', '2', '3', '4', '5', '6', '7', '8', '9':
		if len(expr) > 3 && strings.HasSuffix(expr, "64") {
			if expr[len(expr)-3] == 'u' {
				x, err := strconv.ParseUint(expr[:len(expr)-3], 10, 64)
				if err != nil {
					return Undefined, errSyntax(pos)
				}
				return Uint64(x), nil
			}
			if expr[len(expr)-3] == 'i' {
				x, err := strconv.ParseInt(expr[:len(expr)-3], 10, 64)
				if err != nil {
					return Undefined, errSyntax(pos)
				}
				return Int64(x), nil
			}
		}
		x, err := strconv.ParseFloat(expr, 64)
		if err != nil {
			return Undefined, errSyntax(pos)
		}
		return Float64(x), nil
	case '"', '\'', '`':
		var s string
		var ok bool
		s, raw, ok := parseString(expr)
		if !ok {
			return Undefined, errSyntax(pos)
		}
		left = String(s)
		leftReady = true
		expr = expr[len(raw):]
		pos += len(raw)
	case '(', '{', '[':
		g, err := readGroup(expr, pos)
		if err != nil {
			return Undefined, err
		}
		if g[0] == '(' {
			// paren groups can be evaluated and used as the leading value.
			left, err = evalExpr(g[1:len(g)-1], pos+1, steps, nil, ctx)
			if err != nil {
				return Undefined, err
			}
			leftReady = true
			expr = expr[len(g):]
			pos += len(g)
		} else {
			// '{', '[' not currently allowed as a leading value
			// Perhaps in the future.
			return Undefined, errSyntax(pos)
		}
	}

	var leftIdent string

	if !leftReady {
		// probably a chainable identifier
		ident, ok := readIdent(expr)
		if !ok {
			return Undefined, errSyntax(pos)
		}
		switch ident {
		case "new", "typeof", "void", "await", "in", "instanceof", "yield":
			return Undefined, errSyntax(pos)
		case "true":
			left = Bool(true)
		case "false":
			left = Bool(false)
		case "NaN":
			left = Float64(math.NaN())
		case "Infinity":
			left = Float64(math.Inf(+1))
		case "undefined":
			left = Undefined
		case "null":
			left = Null
		default:
			var err error
			left, err = getRefValue(false, Undefined, ident, pos, false, ctx)
			if err != nil {
				return Undefined, err
			}
		}
		leftReady = true
		expr = expr[len(ident):]
		pos += len(ident)
		leftIdent = ident
	}

	var leftLeft Value
	var hasLeftLeft bool

	// read each chained component
	optChain := false
	for {
		// There are more components to read
		expr, pos = trim(expr, pos)
		if len(expr) == 0 {
			break
		}
		switch expr[0] {
		case '?':
			// Optional chaining
			if len(expr) == 1 || expr[1] != '.' {
				return Undefined, errSyntax(pos)
			}
			expr = expr[1:]
			pos++
			optChain = true
			fallthrough
		case '.':
			// Member Access
			expr = expr[1:]
			pos++
			expr, pos = trim(expr, pos)
			ident, ok := readIdent(expr)
			if !ok {
				return Undefined, errSyntax(pos)
			}
			val, err := getRefValue(true, left, ident, pos, optChain, ctx)
			if err != nil {
				return Undefined, err
			}
			leftLeft = left
			hasLeftLeft = true
			left = val
			expr = expr[len(ident):]
			pos += len(ident)
			leftIdent = ident
		case '(', '[':
			g, err := readGroup(expr, pos)
			if err != nil {
				return Undefined, err
			}
			if g[0] == '(' {
				// Function call
				if left.kind != funcKind {
					return Undefined,
						fmt.Errorf("Uncaught TypeError: %s is not a function",
							leftIdent)
				}
				var val Value
				if ctx != nil && ctx.Extender != nil {
					var info CallInfo
					info.Chain = hasLeftLeft
					info.Value = leftLeft
					info.Ident = left.strVal
					info.Args = Args{expr: g[1 : len(g)-1], ctx: ctx}
					val, err = ctx.Extender.Call(info, ctx)
					if err != nil {
						return Undefined, errCall(err, pos)
					}
				}
				leftLeft = left
				hasLeftLeft = true
				left = val
			} else {
				// Computed Member Access
				last, err := evalExpr(g[1:len(g)-1], 0, steps, nil, ctx)
				if err != nil {
					return Undefined, err
				}
				ident := last.String()
				val, err := getRefValue(true, left, ident, pos, optChain, ctx)
				if err != nil {
					return Undefined, err
				}
				leftLeft = left
				hasLeftLeft = true
				left = val
			}
			expr = expr[len(g):]
			pos += len(g)
		default:
			return Undefined, errSyntax(pos)
		}
	}
	return left, nil
}

type ComputedArgs struct {
	values []Value
}

func (args *ComputedArgs) Get(index int) Value {
	if index < 0 || index >= len(args.values) {
		return Undefined
	}
	return args.values[index]
}

func (args *ComputedArgs) Len() int {
	return len(args.values)
}

type Args struct {
	expr string
	ctx  *Context
}

func (args *Args) Compute() (ComputedArgs, error) {
	var values []Value
	err := args.ForEachValue(func(value Value) error {
		values = append(values, value)
		return nil
	})
	return ComputedArgs{values}, err
}

func (args *Args) ForEachValue(iter func(value Value) error) error {
	_, err := EvalForEach(args.expr, iter, args.ctx)
	return err
}

func getRefValue(chain bool, left Value, ident string, pos int, optChain bool,
	ctx *Context,
) (Value, error) {
	val, err := func() (Value, error) {
		if ctx == nil || ctx.Extender == nil {
			return Undefined, errUndefined(ident, pos, chain)
		}
		info := RefInfo{Chain: chain, Value: left, Ident: ident}
		val, err := ctx.Extender.Ref(info, ctx)
		if err != nil {
			return Undefined, errReference(err, pos)
		}
		if val == Undefined && left == Undefined {
			return Undefined, errUndefined(ident, pos, chain)
		}
		return val, nil
	}()
	if err != nil {
		var skipErr bool
		if optChain {
			if err, ok := err.(*errEval); ok {
				if err.udef {
					skipErr = true
				}
			}
		}
		if !skipErr {
			return Undefined, err
		}
	}
	return val, nil
}

func readIDStart(expr string) (int, bool) {
	if len(expr) == 0 {
		return 0, true
	}
	if expr[0] == '$' || expr[0] == '_' ||
		(expr[0] >= 'A' && expr[0] <= 'Z') ||
		(expr[0] >= 'a' && expr[0] <= 'z') {
		return 1, true
	}
	return 0, false
}

func readIDContinue(expr string) (int, bool) {
	if len(expr) == 0 {
		return 0, true
	}
	if expr[0] == '$' || expr[0] == '_' ||
		(expr[0] >= 'A' && expr[0] <= 'Z') ||
		(expr[0] >= 'a' && expr[0] <= 'z') ||
		(expr[0] >= '0' && expr[0] <= '9') {
		return 1, true
	}
	return 0, false
}

func readIdent(expr string) (ident string, ok bool) {
	// Only ascii identifiers for now
	var i int
	var z int
	z, ok = readIDStart(expr[i:])
	if !ok || z == 0 {
		return "", false
	}
	i += z
	for {
		z, ok = readIDContinue(expr[i:])
		if !ok || z == 0 {
			return expr[:i], true
		}
		i += z
	}
}

// parseString parses a Javascript encoded string.
// Adapted from the GJSON project.
func parseString(data string) (out, raw string, ok bool) {
	var esc bool
	if len(data) < 2 {
		return "", "", false
	}
	qch := data[0]
	for i := 1; i < len(data); i++ {
		if data[i] < ' ' {
			break
		}
		if data[i] == '\\' {
			esc = true
			i++
			if i == len(data) {
				return "", "", false
			}
			switch data[i] {
			case 'u':
				if i+1 < len(data) && data[i+1] == '{' {
					i += 2
					var end bool
					for ; i < len(data); i++ {
						if data[i] == '}' {
							end = true
							break
						}
						if !ishex(data[i]) {
							return "", "", false
						}
					}
					if !end {
						return "", "", false
					}
				} else {
					for j := 0; j < 4; j++ {
						i++
						if i >= len(data) || !ishex(data[i]) {
							return "", "", false
						}
					}
				}
			case 'x':
				for j := 0; j < 2; j++ {
					i++
					if i >= len(data) || !ishex(data[i]) {
						return "", "", false
					}
				}
			}
		} else if data[i] == qch {
			// if i != len(data)-1 {
			// 	return "", "", false
			// }
			s := data[1:i]
			if esc {
				s = unescapeString(s)
			}
			return s, data[:i+1], true
		}
	}
	return "", "", false
}

// runeit returns the rune from the the \uXXXX
func runeit(data string, which byte) (r rune, n int) {
	var x uint64
	if which == 'x' {
		x, _ = strconv.ParseUint(data[:2], 16, 64)
		n = 2
	} else {
		var s, e int
		if data[0] == '{' {
			s = 1
			n = len(data)
			for i := 0; i < len(data); i++ {
				if data[i] == '}' {
					e = i
					n = i + 1
					break
				}
			}
		} else {
			e = 4
			n = 4
		}
		x, _ = strconv.ParseUint(data[s:e], 16, 64)
	}
	return rune(x), n
}

// unescapeString unescapes a Javascript string.
// Adapted from the GJSON project.
// The input data must be prevalidates for correctness, and must only be called
// from the parseString operation.
func unescapeString(data string) string {
	var str = make([]byte, 0, len(data))
	for i := 0; i < len(data); i++ {
		switch {
		default:
			str = append(str, data[i])
		case data[i] == '\\':
			i++
			switch data[i] {
			case '0':
				str = append(str, 0)
			case 'b':
				str = append(str, '\b')
			case 'f':
				str = append(str, '\f')
			case 'n':
				str = append(str, '\n')
			case 'r':
				str = append(str, '\r')
			case 't':
				str = append(str, '\t')
			case 'v':
				str = append(str, '\v')
			case 'u':
				i++
				r, n := runeit(data[i:], 'u')
				i += n
				if utf16.IsSurrogate(r) {
					// need another code
					if len(data[i:]) >= 6 && data[i] == '\\' &&
						data[i+1] == 'u' {
						// we expect it to be correct so just consume it
						i += 2
						r2, n := runeit(data[i:], 'u')
						i += n
						r = utf16.DecodeRune(r, r2)
					}
				}
				// provide enough space to encode the largest utf8 possible
				str = appendRune(str, r)
				i-- // backtrack index by one
			case 'x':
				i++
				r, n := runeit(data[i:], 'x')
				i += n
				str = appendRune(str, r)
				i-- // backtrack index by one
			default:
				str = append(str, data[i])
			}
		}
	}
	return string(str)
}

// // readRune reads a rune from preprocessed data.
// func readRune(data []byte) (r rune, n int) {
// 	if data[0] == '{' {
// 		for i := 1; i < len(data); i++ {
// 			if data[0] == '}' {

// 			}
// 		}
// 	}
// }

func appendRune(dst []byte, r rune) []byte {
	// provide enough space to encode the largest utf8 possible
	dst = append(dst, 0, 0, 0, 0, 0, 0, 0, 0)
	n := utf8.EncodeRune(dst[len(dst)-8:], r)
	return dst[:len(dst)-8+n]
}

func fact(left Value, op byte, expr string, pos, steps int, ctx *Context,
) (Value, error) {
	expr, pos = trim(expr, pos)
	if len(expr) == 0 {
		return Undefined, errSyntax(pos)
	}
	right, err := evalAtom(expr, pos, steps, ctx)
	if err != nil {
		return Undefined, err
	}
	switch op {
	case '*':
		return left.mul(right, pos, ctx)
	case '/':
		return left.div(right, pos, ctx)
	case '%':
		return left.mod(right, pos, ctx)
	default:
		return right, nil
	}
}

func evalFacts(expr string, pos, steps int, ctx *Context) (Value, error) {
	var err error
	var s int
	var left Value
	var op byte
	for i := 0; i < len(expr); i++ {
		switch expr[i] {
		case '*', '/', '%':
			left, err = fact(left, op, expr[s:i], pos+s, steps, ctx)
			if err != nil {
				return Undefined, err
			}
			op = expr[i]
			s = i + 1
		case '(', '[', '{', '"', '\'', '`':
			g, err := readGroup(expr[i:], pos+i)
			if err != nil {
				return Undefined, err
			}
			i = i + len(g) - 1
		}
	}
	return fact(left, op, expr[s:], pos+s, steps, ctx)
}

func sum(left Value, op byte, expr string, neg, end bool, pos, steps int,
	ctx *Context,
) (Value, error) {
	expr, pos = trim(expr, pos)
	if len(expr) == 0 {
		return Undefined, errSyntax(pos)
	}
	// parse factors of expression
	right, err := evalAuto(stepSums<<1, expr, pos, steps, nil, ctx)
	if err != nil {
		return Undefined, err
	}
	if neg {
		// make right negative
		right, err = right.mul(Float64(-1), pos, ctx)
		if err != nil {
			return Undefined, err
		}
	}
	switch op {
	case '+':
		return left.add(right, pos, ctx)
	case '-':
		return left.sub(right, pos, ctx)
	default:
		return right, nil
	}
}

func evalSums(expr string, pos, steps int, ctx *Context) (Value, error) {
	var err error
	var s int
	var left Value
	var op byte
	var fill bool
	var neg bool
	for i := 0; i < len(expr); i++ {
		switch expr[i] {
		case '-', '+':
			if !fill {
				if i > 0 && expr[i-1] == expr[i] {
					// -- not allowed
					return Undefined, errSyntax(pos + i)
				}
				if expr[i] == '-' {
					neg = !neg
				}
				s = i + 1
				continue
			}
			if i > 0 && (expr[i-1] == 'e' || expr[i-1] == 'E') {
				// scientific notation
				continue
			}
			if neg {
				if s > 0 && s < len(expr) && expr[s-1] == '-' &&
					expr[s] >= '0' && expr[s] <= '9' {
					s--
					neg = false
				}
			}
			left, err = sum(left, op, expr[s:i], neg, false, pos+s, steps, ctx)
			if err != nil {
				return Undefined, err
			}
			op = expr[i]
			s = i + 1
			fill = false
			neg = false
		case '(', '[', '{', '"', '\'', '`':
			g, err := readGroup(expr[i:], pos+i)
			if err != nil {
				return Undefined, err
			}
			i = i + len(g) - 1
			fill = true
		default:
			if !fill && !isspace(expr[i]) {
				fill = true
			}
		}
	}
	if neg {
		if s > 0 && s < len(expr) && expr[s-1] == '-' &&
			expr[s] >= '0' && expr[s] <= '9' {
			s--
			neg = false
		}
	}
	return sum(left, op, expr[s:], neg, true, pos+s, steps, ctx)
}

func comp(left Value, op byte, expr string, pos, steps int, ctx *Context,
) (Value, error) {
	expr, pos = trim(expr, pos)
	if len(expr) == 0 {
		return Undefined, errSyntax(pos)
	}
	// parse next expression
	right, err := evalAuto(stepComps<<1, expr, pos, steps, nil, ctx)
	if err != nil {
		return Undefined, err
	}
	switch op {
	case '<':
		return left.lt(right, pos, ctx)
	case '<' + 32:
		return left.lte(right, pos, ctx)
	case '>':
		return left.gt(right, pos, ctx)
	case '>' + 32:
		return left.gte(right, pos, ctx)
	default:
		return right, nil
	}
}

func evalComps(expr string, pos, steps int, ctx *Context) (Value, error) {
	var err error
	var s int
	var left Value
	var op byte
	for i := 0; i < len(expr); i++ {
		switch expr[i] {
		case '<', '>':
			opch := expr[i]
			opsz := 1
			if i < len(expr)-1 && expr[i+1] == '=' {
				opch += 32
				opsz++
			}
			left, err = comp(left, op, expr[s:i], pos+s, steps, ctx)
			if err != nil {
				return Undefined, err
			}
			op = opch
			i = i + opsz - 1
			s = i + 1
		case '(', '[', '{', '"', '\'', '`':
			g, err := readGroup(expr[i:], pos+i)
			if err != nil {
				return Undefined, err
			}
			i = i + len(g) - 1
		}
	}
	return comp(left, op, expr[s:], pos+s, steps, ctx)
}

func equal(left Value, op byte, expr string, pos, steps int, ctx *Context,
) (Value, error) {
	var neg bool
	var boolit bool
	expr, pos = trim(expr, pos)
	for {
		if len(expr) == 0 {
			return Undefined, errSyntax(pos)
		}
		if expr[0] != '!' {
			break
		}
		neg = !neg
		boolit = true
		expr = expr[1:]
		expr, pos = trim(expr, pos+1)
	}
	// parse next expression
	right, err := evalAuto(stepEquality<<1, expr, pos, steps, nil, ctx)
	if err != nil {
		return Undefined, err
	}
	if boolit {
		right = right.tobool()
		if neg {
			right.boolVal = !right.boolVal
		}
	}
	switch op {
	case '=':
		return left.eq(right, pos, ctx)
	case '!':
		return left.neq(right, pos, ctx)
	case '=' + 32:
		return left.seq(right, pos, ctx)
	case '!' + 32:
		return left.sneq(right, pos, ctx)
	default:
		return right, nil
	}
}

func evalEquality(expr string, pos, steps int, ctx *Context) (Value, error) {
	var err error
	var s int
	var left Value
	var op byte
	for i := 0; i < len(expr); i++ {
		switch expr[i] {
		case '=', '!':
			opch := expr[i]
			opsz := 1
			switch opch {
			case '=':
				if i > 0 && (expr[i-1] == '>' || expr[i-1] == '<') {
					continue
				}
				if i == len(expr)-1 || expr[i+1] != '=' {
					return Undefined, errSyntax(pos + i)
				}
				opsz++
			case '!':
				if i == len(expr)-1 || expr[i+1] != '=' {
					continue
				}
				opsz++
			}
			if i+2 < len(expr) && expr[i+2] == '=' {
				// strict
				opch += 32
				opsz++
			}
			left, err = equal(left, op, expr[s:i], pos+s, steps, ctx)
			if err != nil {
				return Undefined, err
			}
			op = opch
			i = i + opsz - 1
			s = i + 1
		case '(', '[', '{', '"', '\'', '`':
			g, err := readGroup(expr[i:], pos+i)
			if err != nil {
				return Undefined, err
			}
			i = i + len(g) - 1
		}
	}
	return equal(left, op, expr[s:], pos+s, steps, ctx)
}

func logicalAnd(left Value, op byte, expr string, pos, steps int, ctx *Context,
) (Value, error) {
	expr, pos = trim(expr, pos)
	if len(expr) == 0 {
		return Undefined, errSyntax(pos)
	}
	right, err := evalAuto(stepLogicalAnd<<1, expr, pos, steps, nil, ctx)
	if err != nil {
		return Undefined, err
	}
	switch op {
	case '&':
		return left.and(right, pos, ctx)
	default:
		return right, nil
	}
}

func evalLogicalAnd(expr string, pos, steps int, ctx *Context) (Value, error) {
	var err error
	var s int
	var left Value
	var op byte
	for i := 0; i < len(expr); i++ {
		switch expr[i] {
		case '&':
			if i == len(expr)-1 || expr[i+1] != expr[i] {
				return Undefined, errSyntax(pos + i)
			}
			left, err = logicalAnd(left, op, expr[s:i], pos+s, steps, ctx)
			if err != nil {
				return Undefined, err
			}
			op = expr[i]
			i++
			s = i + 1
		case '(', '[', '{', '"', '\'', '`':
			g, err := readGroup(expr[i:], pos+i)
			if err != nil {
				return Undefined, err
			}
			i = i + len(g) - 1
		}
	}
	return logicalAnd(left, op, expr[s:], pos+s, steps, ctx)
}

func logicalOr(left Value, op byte, expr string, pos, steps int, ctx *Context,
) (Value, error) {
	expr, pos = trim(expr, pos)
	if len(expr) == 0 {
		return Undefined, errSyntax(pos)
	}
	right, err := evalAuto(stepLogicalOr<<1, expr, pos, steps, nil, ctx)
	if err != nil {
		return Undefined, err
	}
	switch op {
	case '|':
		return left.or(right, pos, ctx)
	case '?':
		return left.coalesce(right, pos, ctx)
	default:
		return right, nil
	}
}

func evalLogicalOr(expr string, pos, steps int, ctx *Context) (Value, error) {
	var err error
	var s int
	var left Value
	var op byte
	for i := 0; i < len(expr); i++ {
		switch expr[i] {
		case '?':
			if i+1 < len(expr) && expr[i+1] == '.' {
				// '?.' operator
				i++
				continue
			}
			fallthrough
		case '|':
			if i == len(expr)-1 || expr[i+1] != expr[i] {
				return Undefined, errSyntax(pos + i)
			}
			left, err = logicalOr(left, op, expr[s:i], pos+s, steps, ctx)
			if err != nil {
				return Undefined, err
			}
			op = expr[i]
			i++
			s = i + 1
		case '(', '[', '{', '"', '\'', '`':
			g, err := readGroup(expr[i:], pos+i)
			if err != nil {
				return Undefined, err
			}
			i = i + len(g) - 1
		}
	}
	return logicalOr(left, op, expr[s:], pos+s, steps, ctx)
}

func evalTerns(expr string, pos, steps int, ctx *Context) (Value, error) {
	var cond string
	var s int
	var depth int
	for i := 0; i < len(expr); i++ {
		switch expr[i] {
		case '?':
			if i+1 < len(expr) && (expr[i+1] == '?' || expr[i+1] == '.') {
				// '??' or '?.' operator
				i++
				continue
			}
			if depth == 0 {
				cond = expr[:i]
				s = i + 1
			}
			depth++
		case ':':
			depth--
			if depth == 0 {
				left := expr[s:i]
				right := expr[i+1:]
				res, err := evalExpr(cond, pos, steps, nil, ctx)
				if err != nil {
					return Undefined, err
				}
				if res.Bool() {
					return evalExpr(left, pos+s, steps, nil, ctx)
				}
				return evalExpr(right, pos+i+1, steps, nil, ctx)
			}
		case '(', '[', '{', '"', '\'', '`':
			g, err := readGroup(expr[i:], pos+i)
			if err != nil {
				return Undefined, err
			}
			i = i + len(g) - 1
		}
	}
	if depth == 0 {
		return evalAuto(stepTerns<<1, expr, pos, steps, nil, ctx)
	}
	return Undefined, errSyntax(pos)
}

func evalComma(expr string, pos, steps int, iter func(value Value) error,
	ctx *Context,
) (Value, error) {
	var s int
	for i := 0; i < len(expr); i++ {
		switch expr[i] {
		case ',':
			res, err := evalAuto(stepComma<<1, expr[s:i], pos+s, steps, nil,
				ctx)
			if err != nil {
				return Undefined, err
			}
			if iter != nil {
				if err := iter(res); err != nil {
					if err == ErrStop {
						return res, nil
					}
					return Undefined, err
				}
			}
			s = i + 1
		case '(', '[', '{', '"', '\'', '`':
			g, err := readGroup(expr[i:], pos+i)
			if err != nil {
				return Undefined, err
			}
			i = i + len(g) - 1
		}
	}
	res, err := evalAuto(stepComma<<1, expr[s:], pos+s, steps, nil, ctx)
	if err != nil {
		return Undefined, err
	}
	if iter != nil {
		if err := iter(res); err != nil {
			if err == ErrStop {
				return res, nil
			}
			return Undefined, err
		}
	}
	return res, nil
}

func evalAuto(step int, expr string, pos, steps int,
	iter func(value Value) error, ctx *Context,
) (Value, error) {
	switch step {
	case stepComma:
		if (steps & stepComma) == stepComma {
			return evalComma(expr, pos, steps, iter, ctx)
		}
		fallthrough
	case stepTerns:
		if (steps & stepTerns) == stepTerns {
			return evalTerns(expr, pos, steps, ctx)
		}
		fallthrough
	case stepLogicalOr:
		if (steps & stepLogicalOr) == stepLogicalOr {
			return evalLogicalOr(expr, pos, steps, ctx)
		}
		fallthrough
	case stepLogicalAnd:
		if (steps & stepLogicalAnd) == stepLogicalAnd {
			return evalLogicalAnd(expr, pos, steps, ctx)
		}
		fallthrough
	case stepEquality:
		if (steps & stepEquality) == stepEquality {
			return evalEquality(expr, pos, steps, ctx)
		}
		fallthrough
	case stepComps:
		if (steps & stepComps) == stepComps {
			return evalComps(expr, pos, steps, ctx)
		}
		fallthrough
	case stepSums:
		if (steps & stepSums) == stepSums {
			return evalSums(expr, pos, steps, ctx)
		}
		fallthrough
	case stepFacts:
		if (steps & stepFacts) == stepFacts {
			return evalFacts(expr, pos, steps, ctx)
		}
		fallthrough
	default:
		return evalAtom(expr, pos, steps, ctx)
	}
}

func evalExpr(expr string, pos, steps int, iter func(value Value) error,
	ctx *Context,
) (Value, error) {
	// terns >> logicals >> comps >> sums >> facts >> atoms
	return evalAuto(stepComma, expr, 0, steps, iter, ctx)
}

type RefInfo struct {
	Chain bool
	Value Value
	Ident string
}

type OpInfo struct {
	Left  Value
	Op    Op
	Right Value
}

type CallInfo struct {
	Chain bool
	Value Value
	Ident string
	Args  Args
}

type Extender interface {
	// ref allows for custom evaluation of an property or variable.
	Ref(info RefInfo, ctx *Context) (Value, error)
	// Op allows for custom opererators on values.
	Op(info OpInfo, ctx *Context) (Value, error)
	// Call allows for function call for custom values.
	Call(info CallInfo, ctx *Context) (Value, error)
}

// Context for Eval
type Context struct {
	UserData any
	Extender Extender
}

// NewExtender is a convenience function for creating a simple extender using
// the provided eval and op functions.
func NewExtender(
	ref func(info RefInfo, ctx *Context) (Value, error),
	call func(info CallInfo, ctx *Context) (Value, error),
	op func(info OpInfo, ctx *Context) (Value, error),
) Extender {
	if ref == nil {
		ref = func(info RefInfo, ctx *Context,
		) (Value, error) {
			return Undefined, nil
		}
	}
	if call == nil {
		call = func(info CallInfo, ctx *Context) (Value, error) {
			return Undefined, nil
		}
	}
	if op == nil {
		op = func(info OpInfo, ctx *Context) (Value, error) {
			return Undefined, nil
		}
	}
	return &simpleExtender{ref, call, op}
}

type simpleExtender struct {
	ref  func(info RefInfo, ctx *Context) (Value, error)
	call func(info CallInfo, ctx *Context) (Value, error)
	op   func(info OpInfo, ctx *Context) (Value, error)
}

func (e *simpleExtender) Ref(info RefInfo, ctx *Context) (Value, error) {
	return e.ref(info, ctx)
}

func (e *simpleExtender) Call(info CallInfo, ctx *Context) (Value, error) {
	return e.call(info, ctx)
}

func (e *simpleExtender) Op(op OpInfo, ctx *Context) (Value, error) {
	return e.op(op, ctx)
}

// Operator Precedence
// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence
const (
	_              = 1 << iota //
	stepComma                  //  1: Comma / Sequence
	stepTerns                  //  3: Conditional (ternary) operator
	stepLogicalOr              //  4: Logical OR (||) Nullish coalescing operator (??)
	stepLogicalAnd             //  5: Logical AND (&&)
	stepEquality               //  9: Equality (==) (!=)
	stepComps                  // 10: Comparison (<) (<=) (>) (>=)
	stepSums                   // 12: Summation (-) (+)
	stepFacts                  // 13: Factors (*) (/)
)

var opSteps = [256]uint16{
	',': stepComma,                 // ','
	'?': stepTerns | stepLogicalOr, // '?:' '??'
	':': stepTerns,                 // '?:'
	'|': stepLogicalOr,             // '||'
	'&': stepLogicalAnd,            // '&&'
	'=': stepComps | stepEquality,  // '==' '<=' '>='
	'!': stepEquality,              // '!' '!='
	'<': stepComps,                 // '<' '<='
	'>': stepComps,                 // '>' '>='
	'+': stepSums,                  // '+'
	'-': stepSums,                  // '-'
	'*': stepFacts,                 // '*'
	'/': stepFacts,                 // '/'
	'%': stepFacts,                 // '%'
}

// Eval evaluates an expression and returns the Result.
func Eval(expr string, ctx *Context) (Value, error) {
	return EvalForEach(expr, nil, ctx)
}

// EvalForEach iterates over a series of comma delimited expressions.
// The last value in the series is returned.
// Returning ErrStop will stop the iteration early and return the last known
// value and nil as an error.
// Returning any other error from iter will stop the iteration and return the
// same error.
func EvalForEach(expr string, iter func(value Value) error, ctx *Context,
) (Value, error) {
	var pos int
	expr, pos = trim(expr, 0)
	if len(expr) == 0 {
		return Undefined, nil
	}
	// Determine which steps are (possibly) needed by scanning every byte in
	// the input expression and looking for potential candidate characters.
	var steps int
	for i := 0; i < len(expr); i++ {
		steps |= int(opSteps[expr[i]])
	}
	if iter != nil {
		// require the comma step when using an iterator.
		steps |= stepComma
	}
	r, err := evalExpr(expr, pos, steps, iter, ctx)
	if err != nil {
		return Undefined, err
	}
	return r, nil
}

func readGroup(data string, pos int) (string, error) {
	g, ok := squash(data)
	if !ok {
		return "", errSyntax(pos)
	}
	if len(g) < 2 || g[len(g)-1] != closech(data[0]) {
		return "", errSyntax(pos)
	}
	return g, nil
}

func squash(data string) (string, bool) {
	// expects that the lead character is
	//   '[' or '{' or '(' or '"' or '\'' or '`'
	// squash the value, ignoring all nested arrays and objects.
	var i, depth int
	switch data[0] {
	case '"', '\'', '`':
	default:
		i, depth = 1, 1
	}
	for ; i < len(data); i++ {
		if data[i] < '"' || data[i] > '}' {
			continue
		}
		switch data[i] {
		case '"', '\'', '`':
			qch := data[i]
			i++
			s2 := i
			for ; i < len(data); i++ {
				if data[i] > '\\' {
					continue
				}
				if data[i] == qch {
					// look for an escaped slash
					if data[i-1] == '\\' {
						n := 0
						for j := i - 2; j > s2-1; j-- {
							if data[j] != '\\' {
								break
							}
							n++
						}
						if n%2 == 0 {
							continue
						}
					}
					break
				}
			}
			if depth == 0 {
				if i >= len(data) {
					return data, false
				}
				return data[:i+1], true
			}
		case '{', '[', '(':
			depth++
		case '}', ']', ')':
			depth--
			if depth == 0 {
				return data[:i+1], true
			}
		}
	}
	return data, false
}

var chars = [256]uint8{
	'\t': 1, '\n': 1, '\v': 1, '\f': 1, '\r': 1, ' ': 1, // space
	'0': 2, '1': 2, '2': 2, '3': 2, '4': 2, '5': 2, '6': 2, '7': 2, // hex
	'8': 2, '9': 2, 'a': 2, 'b': 2, 'c': 2, 'd': 2, 'e': 2, 'f': 2,
	'A': 2, 'B': 2, 'C': 2, 'D': 2, 'E': 2, 'F': 2,
}

func isspace(c byte) bool {
	return chars[c] == 1
}

func ishex(c byte) bool {
	return chars[c] == 2
}

// trim a simple ascii string along with doing position counting.
// This is a tad bit faster than strings.TrimSpace.
func trim(s string, pos int) (string, int) {
	for len(s) > 0 && isspace(s[0]) {
		s = s[1:]
		pos++
	}
	for len(s) > 0 && isspace(s[len(s)-1]) {
		s = s[:len(s)-1]
	}
	return s, pos
}
