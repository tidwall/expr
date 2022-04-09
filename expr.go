// Copyright 2022 Joshua J Baker. All rights reserved.
// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file.

package expr

import (
	"errors"
	"fmt"
	"math"
	"strconv"
	"unicode/utf16"
	"unicode/utf8"
)

func errSyntax(pos int) error {
	return &errEval{pos: pos, err: errors.New("SyntaxError")}
}

func errUndefined(expr string, pos int) error {
	return &errEval{
		pos: pos, err: fmt.Errorf("ReferenceError: %s is not defined", expr),
	}
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

var ErrUndefined = errors.New("undefined")
var ErrStop = errors.New("stop")

type int64er interface{ Int64() int64 }
type uint64er interface{ Uint64() uint64 }
type float64er interface{ Float64() float64 }
type booler interface{ Bool() bool }
type stringer interface{ String() string }

type errEval struct {
	pos int
	err error
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
	Undefined = Value{kind: undf}
	Null      = Value{kind: nval}
)

// Op is an operator for Custom values used for the Options.Op function.
type Op int

const (
	_      Op = iota
	OpAdd     // +
	OpSub     // -
	OpMul     // *
	OpDiv     // /
	OpMod     // %
	OpLt      // <
	OpLte     // <=
	OpGt      // >
	OpGte     // >=
	OpEq      // ==
	OpNeq     // !=
	OpAnd     // &&
	OpOr      // ||
	OpCoal    // ??
)

func (op Op) String() string {
	switch op {
	case OpAdd:
		return "+"
	case OpSub:
		return "-"
	case OpMul:
		return "*"
	case OpDiv:
		return "/"
	case OpMod:
		return "%"
	case OpLt:
		return "<"
	case OpLte:
		return "<="
	case OpGt:
		return ">"
	case OpGte:
		return ">="
	case OpEq:
		return "=="
	case OpNeq:
		return "!="
	case OpAnd:
		return "&&"
	case OpOr:
		return "||"
	case OpCoal:
		return "??"
	default:
		return ""
	}
}

type kind byte

const (
	undf kind = iota // undefined
	nval             // null
	bval             // bool
	fval             // float64
	ival             // int64
	uval             // uint64
	sval             // string
	cval             // custom
)

// Value represents is the return value of Eval.
type Value struct {
	kind kind        // kind
	bval bool        // bool
	fval float64     // float64
	ival int64       // int64
	uval uint64      // uint64
	sval string      // string
	cval interface{} // custom value
}

// String returns a string value.
func String(s string) Value { return Value{kind: sval, sval: s} }

// Number returns a float64 value.
func Number(x float64) Value { return Float64(x) }

// Bool returns a bool value.
func Bool(t bool) Value { return Value{kind: bval, bval: t} }

// Float64 returns an int64 value.
func Float64(x float64) Value { return Value{kind: fval, fval: x} }

// Int64 returns an int64 value.
func Int64(x int64) Value { return Value{kind: ival, ival: x} }

// Uint64 returns a uint64 value.
func Uint64(x uint64) Value { return Value{kind: uval, uval: x} }

// Custom returns a custom user-defined value.
func Custom(v interface{}) Value { return Value{kind: cval, cval: v} }

func (a Value) IsCustom() bool {
	return a.kind == cval
}

func doOp(op Op, a, b Value, pos int, ctx *Context) (Value, error) {
	if ctx != nil && ctx.Extender != nil {
		v, err := ctx.Extender.Op(op, a, b, ctx)
		if err == nil {
			return v, nil
		}
		if err != ErrUndefined {
			return Undefined, errOperator(err, pos)
		}
	}
	return Undefined, errOperator(errors.New("undefined"), pos)
}

func (a Value) add(b Value, pos int, ctx *Context) (Value, error) {
	if a.kind == cval || b.kind == cval {
		return doOp(OpAdd, a, b, pos, ctx)
	}
	if a.kind == b.kind {
		switch a.kind {
		case fval:
			return Value{kind: fval, fval: a.fval + b.fval}, nil
		case ival:
			return Value{kind: ival, ival: a.ival + b.ival}, nil
		case uval:
			return Value{kind: uval, uval: a.uval + b.uval}, nil
		case sval:
			return Value{kind: sval, sval: a.sval + b.sval}, nil
		case bval, undf, nval:
			a, b = a.tofval(), b.tofval()
			return Value{kind: fval, fval: a.fval + b.fval}, nil
		}
	} else if a.isnum() && b.isnum() {
		a, b = a.tofval(), b.tofval()
		return Value{kind: fval, fval: a.fval + b.fval}, nil
	}
	a, b = a.tostr(), b.tostr()
	return Value{kind: sval, sval: a.sval + b.sval}, nil
}

func (a Value) isnum() bool {
	switch a.kind {
	case fval, ival, uval, bval, nval, undf:
		return true
	}
	return false
}

func (a Value) sub(b Value, pos int, ctx *Context) (Value, error) {
	if a.kind == cval || b.kind == cval {
		return doOp(OpSub, a, b, pos, ctx)
	}
	if a.kind == b.kind {
		switch a.kind {
		case fval:
			return Value{kind: fval, fval: a.fval - b.fval}, nil
		case ival:
			return Value{kind: ival, ival: a.ival - b.ival}, nil
		case uval:
			return Value{kind: uval, uval: a.uval - b.uval}, nil
		}
	}
	a, b = a.tofval(), b.tofval()
	return Value{kind: fval, fval: a.fval - b.fval}, nil
}

func (a Value) mul(b Value, pos int, ctx *Context) (Value, error) {
	if a.kind == cval || b.kind == cval {
		return doOp(OpMul, a, b, pos, ctx)
	}
	if a.kind == b.kind {
		switch a.kind {
		case fval:
			return Value{kind: fval, fval: a.fval * b.fval}, nil
		case ival:
			return Value{kind: ival, ival: a.ival * b.ival}, nil
		case uval:
			return Value{kind: uval, uval: a.uval * b.uval}, nil
		}
	}
	a, b = a.tofval(), b.tofval()
	return Value{kind: fval, fval: a.fval * b.fval}, nil
}

func (a Value) div(b Value, pos int, ctx *Context) (Value, error) {
	if a.kind == cval || b.kind == cval {
		return doOp(OpDiv, a, b, pos, ctx)
	}
	if a.kind == b.kind {
		switch a.kind {
		case fval:
			return Value{kind: fval, fval: a.fval / b.fval}, nil
		case ival:
			return Value{kind: ival, ival: a.ival / b.ival}, nil
		case uval:
			return Value{kind: uval, uval: a.uval / b.uval}, nil
		}
	}
	a, b = a.tofval(), b.tofval()
	return Value{kind: fval, fval: a.fval / b.fval}, nil
}
func (a Value) mod(b Value, pos int, ctx *Context) (Value, error) {
	if a.kind == cval || b.kind == cval {
		return doOp(OpMod, a, b, pos, ctx)
	}
	if a.kind == b.kind {
		switch a.kind {
		case fval:
			return Value{kind: fval, fval: math.Mod(a.fval, b.fval)}, nil
		case ival:
			return Value{kind: ival, ival: a.ival % b.ival}, nil
		case uval:
			return Value{kind: uval, uval: a.uval % b.uval}, nil
		}
	}
	a, b = a.tofval(), b.tofval()
	return Value{kind: fval, fval: math.Mod(a.fval, b.fval)}, nil
}

func (a Value) lt(b Value, pos int, ctx *Context) (Value, error) {
	if a.kind == cval || b.kind == cval {
		return doOp(OpLt, a, b, pos, ctx)
	}
	if a.kind == b.kind {
		switch a.kind {
		case fval:
			return Value{kind: bval, bval: a.fval < b.fval}, nil
		case ival:
			return Value{kind: bval, bval: a.ival < b.ival}, nil
		case uval:
			return Value{kind: bval, bval: a.uval < b.uval}, nil
		case sval:
			return Value{kind: bval, bval: a.sval < b.sval}, nil
		}
	}
	a, b = a.tofval(), b.tofval()
	return Value{kind: bval, bval: a.fval < b.fval}, nil
}

func (a Value) lte(b Value, pos int, ctx *Context) (Value, error) {
	if a.kind == cval || b.kind == cval {
		return doOp(OpLte, a, b, pos, ctx)
	}
	if a.kind == b.kind {
		switch a.kind {
		case fval:
			return Value{kind: bval, bval: a.fval <= b.fval}, nil
		case ival:
			return Value{kind: bval, bval: a.ival <= b.ival}, nil
		case uval:
			return Value{kind: bval, bval: a.uval <= b.uval}, nil
		case sval:
			return Value{kind: bval, bval: a.sval <= b.sval}, nil
		}
	}
	a, b = a.tofval(), b.tofval()
	return Value{kind: bval, bval: a.fval <= b.fval}, nil
}

func (a Value) gt(b Value, pos int, ctx *Context) (Value, error) {
	if a.kind == cval || b.kind == cval {
		return doOp(OpGt, a, b, pos, ctx)
	}
	if a.kind == b.kind {
		switch a.kind {
		case fval:
			return Value{kind: bval, bval: a.fval > b.fval}, nil
		case ival:
			return Value{kind: bval, bval: a.ival > b.ival}, nil
		case uval:
			return Value{kind: bval, bval: a.uval > b.uval}, nil
		case sval:
			return Value{kind: bval, bval: a.sval > b.sval}, nil
		}
	}
	a, b = a.tofval(), b.tofval()
	return Value{kind: bval, bval: a.fval > b.fval}, nil
}
func (a Value) gte(b Value, pos int, ctx *Context) (Value, error) {
	if a.kind == cval || b.kind == cval {
		return doOp(OpGte, a, b, pos, ctx)
	}
	if a.kind == b.kind {
		switch a.kind {
		case fval:
			return Value{kind: bval, bval: a.fval >= b.fval}, nil
		case ival:
			return Value{kind: bval, bval: a.ival >= b.ival}, nil
		case uval:
			return Value{kind: bval, bval: a.uval >= b.uval}, nil
		case sval:
			return Value{kind: bval, bval: a.sval >= b.sval}, nil
		}
	}
	a, b = a.tofval(), b.tofval()
	return Value{kind: bval, bval: a.fval >= b.fval}, nil
}
func (a Value) eq(b Value, pos int, ctx *Context) (Value, error) {
	if a.kind == cval || b.kind == cval {
		return doOp(OpEq, a, b, pos, ctx)
	}
	if a.kind == b.kind {
		switch a.kind {
		case fval:
			return Value{kind: bval, bval: a.fval == b.fval}, nil
		case ival:
			return Value{kind: bval, bval: a.ival == b.ival}, nil
		case uval:
			return Value{kind: bval, bval: a.uval == b.uval}, nil
		case sval:
			return Value{kind: bval, bval: a.sval == b.sval}, nil
		}
	}
	a, b = a.tofval(), b.tofval()
	return Value{kind: bval, bval: a.fval == b.fval}, nil
}
func (a Value) neq(b Value, pos int, ctx *Context) (Value, error) {
	if a.kind == cval || b.kind == cval {
		return doOp(OpNeq, a, b, pos, ctx)
	}
	if a.kind == b.kind {
		switch a.kind {
		case fval:
			return Value{kind: bval, bval: a.fval != b.fval}, nil
		case ival:
			return Value{kind: bval, bval: a.ival != b.ival}, nil
		case uval:
			return Value{kind: bval, bval: a.uval != b.uval}, nil
		case sval:
			return Value{kind: bval, bval: a.sval != b.sval}, nil
		}
	}
	a, b = a.tofval(), b.tofval()
	return Value{kind: bval, bval: a.fval != b.fval}, nil
}

func (a Value) and(b Value, pos int, ctx *Context) (Value, error) {
	if a.kind == cval || b.kind == cval {
		return doOp(OpAnd, a, b, pos, ctx)
	}
	a, b = a.tobool(), b.tobool()
	return Value{kind: bval, bval: a.bval && b.bval}, nil
}

func (a Value) or(b Value, pos int, ctx *Context) (Value, error) {
	if a.kind == cval || b.kind == cval {
		return doOp(OpOr, a, b, pos, ctx)
	}
	a, b = a.tobool(), b.tobool()
	return Value{kind: bval, bval: a.bval || b.bval}, nil
}

func (a Value) coalesce(b Value, pos int, ctx *Context) (Value, error) {
	if a.kind == cval || b.kind == cval {
		return doOp(OpCoal, a, b, pos, ctx)
	}
	switch a.kind {
	case undf, nval:
		return b, nil
	}
	return a, nil
}

func (a Value) tostr() Value {
	switch a.kind {
	case nval:
		return Value{kind: sval, sval: "null"}
	case bval:
		return Value{kind: sval, sval: strconv.FormatBool(a.bval)}
	case fval:
		var s string
		if math.IsInf(a.fval, 0) {
			if a.fval < 0 {
				s = "-Infinity"
			} else {
				s = "Infinity"
			}
		} else {
			s = strconv.FormatFloat(a.fval, 'f', -1, 64)
		}
		return Value{kind: sval, sval: s}
	case ival:
		return Value{kind: sval, sval: strconv.FormatInt(a.ival, 10)}
	case uval:
		return Value{kind: sval, sval: strconv.FormatUint(a.uval, 10)}
	case sval:
		return a
	case cval:
		if v, ok := a.cval.(stringer); ok {
			return Value{kind: sval, sval: v.String()}
		}
		return Value{kind: sval, sval: fmt.Sprint(a.cval)}
	default:
		return Value{kind: sval, sval: "undefined"}
	}
}

func (a Value) tofval() Value {
	switch a.kind {
	case nval:
		return Value{kind: fval, fval: 0}
	case bval:
		if a.bval {
			return Value{kind: fval, fval: 1}
		}
		return Value{kind: fval, fval: 0}
	case fval:
		return a
	case ival:
		return Value{kind: fval, fval: float64(a.ival)}
	case uval:
		return Value{kind: fval, fval: float64(a.uval)}
	case sval:
		x, err := strconv.ParseFloat(a.sval, 64)
		if err != nil {
			break
		}
		return Value{kind: fval, fval: x}
	case cval:
		if v, ok := a.cval.(float64er); ok {
			return Value{kind: fval, fval: v.Float64()}
		}
		return a.tostr().tofval()
	}
	return Value{kind: fval, fval: math.NaN()}
}

func (a Value) tobool() Value {
	if a.kind == bval {
		return a
	}
	var t bool
	switch a.kind {
	case sval:
		t = a.sval != ""
	case cval:
		if v, ok := a.cval.(booler); ok {
			t = v.Bool()
		} else {
			t = a.tofval().fval != 0
		}
	case undf, nval:
		t = false
	default:
		t = a.tofval().fval != 0
	}
	return Value{kind: bval, bval: t}
}

// Bool returns a boolean representation.
func (a Value) Bool() bool {
	return a.tobool().bval
}

// String returns a string representation.
func (a Value) String() string {
	return a.tostr().sval
}

// Number returns s float64 representation.
func (a Value) Number() float64 {
	return a.Float64()
}

// Float64 returns s float64 representation.
func (a Value) Float64() float64 {
	return a.tofval().fval
}

// Int64 returns an int64 representation.
func (a Value) Int64() int64 {
	switch a.kind {
	case ival:
		return a.ival
	case uval:
		return int64(a.uval)
	case cval:
		if v, ok := a.cval.(int64er); ok {
			return v.Int64()
		}
	}
	return int64(a.tofval().fval)
}

// Uint64 returns a uint64 representation.
func (a Value) Uint64() uint64 {
	switch a.kind {
	case ival:
		return uint64(a.ival)
	case uval:
		return a.uval
	case cval:
		if v, ok := a.cval.(uint64er); ok {
			return v.Uint64()
		}
	}
	return uint64(a.tofval().fval)
}

// Value returns the native Go representation, which is one of the following:
//
//    bool, int64, uint64, float64, string, or nil (if undefined)
//
func (a Value) Value() interface{} {
	switch a.kind {
	case cval:
		return a.cval
	case bval:
		return a.bval
	case fval:
		return a.fval
	case ival:
		return a.ival
	case uval:
		return a.uval
	case sval:
		return a.sval
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
	switch expr[0] {
	case '0':
		if len(expr) > 1 && (expr[1] == 'x' || expr[1] == 'X') {
			x, err := strconv.ParseUint(expr[2:], 16, 64)
			if err != nil {
				return Undefined, errSyntax(pos)
			}
			return Float64(float64(x)), nil
		}
		fallthrough
	case '.', '1', '2', '3', '4', '5', '6', '7', '8', '9':
		x, err := strconv.ParseFloat(expr, 64)
		if err != nil {
			return Undefined, errSyntax(pos)
		}
		return Float64(x), nil
	case '"', '\'', '`':
		s, ok := parseString(expr)
		if !ok {
			return Undefined, errSyntax(pos)
		}
		return String(s), nil
	case 't':
		if expr == "true" {
			return Bool(true), nil
		}
	case 'f':
		if expr == "false" {
			return Bool(false), nil
		}
	case 'N':
		if expr == "NaN" {
			return Float64(math.NaN()), nil
		}
	case 'I':
		if expr == "Infinity" {
			return Float64(math.Inf(+1)), nil
		}
	case 'u':
		if expr == "undefined" {
			return Undefined, nil
		}
	case 'n':
		if expr == "null" {
			return Null, nil
		}
	case '(':
		// grouping
		if expr[len(expr)-1] != ')' {
			return Undefined, errSyntax(pos)
		}
		return evalExpr(expr[1:len(expr)-1], pos+1, steps, nil, ctx)
	}
	if ctx != nil && ctx.Extender != nil {
		res, err := ctx.Extender.Eval(expr, ctx)
		if err != nil {
			if err == ErrUndefined {
				return Undefined, errUndefined(expr, pos)
			}
			return Undefined, errReference(err, pos)
		}
		return res, nil
	}
	return Undefined, errUndefined(expr, pos)
}

// parseString parses a Javascript encoded string.
// Adapted from the GJSON project.
func parseString(data string) (out string, ok bool) {
	var esc bool
	if len(data) < 2 {
		return "", false
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
				return "", false
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
							return "", false
						}
					}
					if !end {
						return "", false
					}
				} else {
					for j := 0; j < 4; j++ {
						i++
						if i >= len(data) || !ishex(data[i]) {
							return "", false
						}
					}
				}
			case 'x':
				for j := 0; j < 2; j++ {
					i++
					if i >= len(data) || !ishex(data[i]) {
						return "", false
					}
				}
			}
		} else if data[i] == qch {
			if i != len(data)-1 {
				return "", false
			}
			s := data[1:i]
			if esc {
				s = unescapeString(s)
			}
			return s, true
		}
	}
	return "", false
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
			right.bval = !right.bval
		}
	}
	switch op {
	case '=':
		return left.eq(right, pos, ctx)
	case '!':
		return left.neq(right, pos, ctx)
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

type Extender interface {
	// Eval allows for custom evaluation of an expression.
	Eval(expr string, ctx *Context) (Value, error)
	// Op allows for custom evaluation of an expression.
	Op(op Op, a, b Value, ctx *Context) (Value, error)
}

// Context for Eval
type Context struct {
	UserData any
	Extender Extender
}

// NewExtender is a convenience function for creating a simple extender using
// the provided eval and op functions.
func NewExtender(
	eval func(expr string, ctx *Context) (Value, error),
	op func(op Op, a, b Value, ctx *Context) (Value, error),
) Extender {
	if eval == nil {
		eval = func(expr string, ctx *Context) (Value, error) {
			return Undefined, ErrUndefined
		}
	}
	if op == nil {
		op = func(op Op, a, b Value, ctx *Context) (Value, error) {
			return Undefined, ErrUndefined
		}
	}
	return &simpleExtender{eval, op}
}

type simpleExtender struct {
	eval func(expr string, ctx *Context) (Value, error)
	op   func(op Op, a, b Value, ctx *Context) (Value, error)
}

func (e *simpleExtender) Eval(expr string, ctx *Context) (Value, error) {
	return e.eval(expr, ctx)
}

func (e *simpleExtender) Op(op Op, a, b Value, ctx *Context) (Value, error) {
	return e.op(op, a, b, ctx)
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
