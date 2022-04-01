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

// Undefined value
var Undefined Value

// Op is an operator for Custom values used for the Options.Op function.
type Op int

const (
	_     Op = iota
	OpAdd    // +
	OpSub    // -
	OpMul    // *
	OpDiv    // /
	OpMod    // %
	OpLt     // <
	OpLte    // <=
	OpGt     // >
	OpGte    // >=
	OpEq     // ==
	OpNeq    // !=
	OpAnd    // &&
	OpOr     // ||
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
	default:
		return ""
	}
}

type kind byte

const (
	undf kind = iota // undefined
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

func doOp(op Op, a, b Value, pos int, opts *Options) (Value, error) {
	// if opts == nil || opts.Extender == nil {
	// 	return Undefined, errOperator(errors.New("undefined"), pos)
	// }
	v, err := opts.Extender.Op(op, a, b, opts.UserData)
	if err != nil {
		return Undefined, errOperator(err, pos)
	}
	if v.kind == undf {
		return Undefined, errOperator(errors.New("undefined"), pos)
	}
	return v, nil
}

func (a Value) add(b Value, pos int, opts *Options) (Value, error) {
	if a.kind == cval || b.kind == cval {
		return doOp(OpAdd, a, b, pos, opts)
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
		case bval:
			return a.tofval().add(b.tofval(), pos, opts)
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
	case fval, ival, uval, bval:
		return true
	}
	return false
}

func (a Value) sub(b Value, pos int, opts *Options) (Value, error) {
	if a.kind == cval || b.kind == cval {
		return doOp(OpSub, a, b, pos, opts)
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

func (a Value) mul(b Value, pos int, opts *Options) (Value, error) {
	if a.kind == cval || b.kind == cval {
		return doOp(OpMul, a, b, pos, opts)
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

func (a Value) div(b Value, pos int, opts *Options) (Value, error) {
	if a.kind == cval || b.kind == cval {
		return doOp(OpDiv, a, b, pos, opts)
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
func (a Value) mod(b Value, pos int, opts *Options) (Value, error) {
	if a.kind == cval || b.kind == cval {
		return doOp(OpMod, a, b, pos, opts)
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

func (a Value) lt(b Value, pos int, opts *Options) (Value, error) {
	if a.kind == cval || b.kind == cval {
		return doOp(OpLt, a, b, pos, opts)
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

func (a Value) lte(b Value, pos int, opts *Options) (Value, error) {
	if a.kind == cval || b.kind == cval {
		return doOp(OpLte, a, b, pos, opts)
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

func (a Value) gt(b Value, pos int, opts *Options) (Value, error) {
	if a.kind == cval || b.kind == cval {
		return doOp(OpGt, a, b, pos, opts)
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
func (a Value) gte(b Value, pos int, opts *Options) (Value, error) {
	if a.kind == cval || b.kind == cval {
		return doOp(OpGte, a, b, pos, opts)
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
func (a Value) eq(b Value, pos int, opts *Options) (Value, error) {
	if a.kind == cval || b.kind == cval {
		return doOp(OpEq, a, b, pos, opts)
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
func (a Value) neq(b Value, pos int, opts *Options) (Value, error) {
	if a.kind == cval || b.kind == cval {
		return doOp(OpNeq, a, b, pos, opts)
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

func (a Value) and(b Value, pos int, opts *Options) (Value, error) {
	if a.kind == cval || b.kind == cval {
		return doOp(OpAnd, a, b, pos, opts)
	}
	a, b = a.tobool(), b.tobool()
	return Value{kind: bval, bval: a.bval && b.bval}, nil
}

func (a Value) or(b Value, pos int, opts *Options) (Value, error) {
	if a.kind == cval || b.kind == cval {
		return doOp(OpOr, a, b, pos, opts)
	}
	a, b = a.tobool(), b.tobool()
	return Value{kind: bval, bval: a.bval || b.bval}, nil
}

func (a Value) tostr() Value {
	switch a.kind {
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
	}
	return Undefined
}

func (a Value) tofval() Value {
	switch a.kind {
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

func evalAtom(expr string, pos, steps int, opts *Options) (Value, error) {
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
			return Uint64(x), nil
		}
		fallthrough
	case '.', '1', '2', '3', '4', '5', '6', '7', '8', '9':
		x, err := strconv.ParseFloat(expr, 64)
		if err != nil {
			return Undefined, errSyntax(pos)
		}
		return Float64(x), nil
	case '"':
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
	case '(':
		// grouping
		if expr[len(expr)-1] != ')' {
			return Undefined, errSyntax(pos)
		}
		return evalAuto(stepLogicals, expr[1:len(expr)-1], pos+1, steps, opts)
	}
	if opts != nil && opts.Extender != nil {
		res, err := opts.Extender.Eval(expr, opts.UserData)
		if err != nil {
			return Undefined, errReference(err, pos)
		}
		if res.kind != undf {
			return res, nil
		}
		// fallthrough
	}
	return Undefined, errUndefined(expr, pos)
}

// parseString parses a Javascript encoded string.
// Adapted from the GJSON project.
func parseString(data string) (out string, ok bool) {
	var esc bool
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
			default:
				return "", false
			case '"', '\\', '/', 'b', 'f', 'n', 'r', 't':
			case 'u':
				for j := 0; j < 4; j++ {
					i++
					if i >= len(data) ||
						(!((data[i] >= '0' && data[i] <= '9') ||
							(data[i] >= 'a' && data[i] <= 'f') ||
							(data[i] >= 'A' && data[i] <= 'F'))) {
						return "", false
					}
				}
			}
		} else if data[i] == '"' {
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
func runeit(data string) rune {
	n, _ := strconv.ParseUint(data[:4], 16, 64)
	return rune(n)
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
			case '\\':
				str = append(str, '\\')
			case '/':
				str = append(str, '/')
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
			case '"':
				str = append(str, '"')
			case 'u':
				r := runeit(data[i+1:])
				i += 5
				if utf16.IsSurrogate(r) {
					// need another code
					if len(data[i:]) >= 6 && data[i] == '\\' &&
						data[i+1] == 'u' {
						// we expect it to be correct so just consume it
						r = utf16.DecodeRune(r, runeit(data[i+2:]))
						i += 6
					}
				}
				// provide enough space to encode the largest utf8 possible
				str = append(str, 0, 0, 0, 0, 0, 0, 0, 0)
				n := utf8.EncodeRune(str[len(str)-8:], r)
				str = str[:len(str)-8+n]
				i-- // backtrack index by one
			}
		}
	}
	return string(str)
}

func fact(left Value, op byte, expr string, pos, steps int, opts *Options,
) (Value, error) {
	expr, pos = trim(expr, pos)
	if len(expr) == 0 {
		return Undefined, errSyntax(pos)
	}
	var right Value
	var err error
	switch expr[0] {
	case '(':
		// parse subexpression
		right, err = evalExpr(expr[1:len(expr)-1], pos+1, steps, opts)
	default:
		// atom
		right, err = evalAtom(expr, pos, steps, opts)
	}
	if err != nil {
		return Undefined, err
	}
	switch op {
	case '*':
		return left.mul(right, pos, opts)
	case '/':
		return left.div(right, pos, opts)
	case '%':
		return left.mod(right, pos, opts)
	default:
		return right, nil
	}
}

func evalFacts(expr string, pos, steps int, opts *Options) (Value, error) {
	var err error
	var s int
	var left Value
	var op byte
	for i := 0; i < len(expr); i++ {
		switch expr[i] {
		case '*', '/', '%':
			left, err = fact(left, op, expr[s:i], pos+s, steps, opts)
			if err != nil {
				return Undefined, err
			}
			op = expr[i]
			s = i + 1
		case '(', '[', '{', '"':
			g, err := readGroup(expr[i:], pos+i)
			if err != nil {
				return Undefined, err
			}
			i = i + len(g) - 1
		}
	}
	return fact(left, op, expr[s:], pos+s, steps, opts)
}

func sum(left Value, op byte, expr string, neg, end bool, pos, steps int,
	opts *Options,
) (Value, error) {
	expr, pos = trim(expr, pos)
	if len(expr) == 0 {
		return Undefined, errSyntax(pos)
	}
	// parse factors of expression
	right, err := evalAuto(stepFacts, expr, pos, steps, opts)
	if err != nil {
		return Undefined, err
	}
	if neg {
		// make right negative
		right, err = right.mul(Float64(-1), pos, opts)
		if err != nil {
			return Undefined, err
		}
	}
	switch op {
	case '+':
		return left.add(right, pos, opts)
	case '-':
		return left.sub(right, pos, opts)
	default:
		return right, nil
	}
}

func evalSums(expr string, pos, steps int, opts *Options) (Value, error) {
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
			left, err = sum(left, op, expr[s:i], neg, false, pos+s, steps,
				opts)
			if err != nil {
				return Undefined, err
			}
			op = expr[i]
			s = i + 1
			fill = false
			neg = false
		case '(', '[', '{', '"':
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
	return sum(left, op, expr[s:], neg, true, pos+s, steps, opts)
}

func comp(left Value, op byte, expr string, pos, steps int, opts *Options,
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
	// parse sums of expression
	right, err := evalAuto(stepSums, expr, pos, steps, opts)
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
	case '<':
		return left.lt(right, pos, opts)
	case '<' + 32:
		return left.lte(right, pos, opts)
	case '>':
		return left.gt(right, pos, opts)
	case '>' + 32:
		return left.gte(right, pos, opts)
	case '=':
		return left.eq(right, pos, opts)
	case '!':
		return left.neq(right, pos, opts)
	default:
		return right, nil
	}
}

func evalComps(expr string, pos, steps int, opts *Options) (Value, error) {
	var err error
	var s int
	var left Value
	var op byte
	for i := 0; i < len(expr); i++ {
		switch expr[i] {
		case '<', '>', '=', '!':
			opch := expr[i]
			opsz := 1
			switch opch {
			case '<', '>':
				if i < len(expr)-1 && expr[i+1] == '=' {
					opch += 32 // '<' becomes '\' and '>' becomes '^'
					opsz++
				}
			case '=':
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
			left, err = comp(left, op, expr[s:i], pos+s, steps, opts)
			if err != nil {
				return Undefined, err
			}
			op = opch
			i = i + opsz - 1
			s = i + 1
		case '(', '[', '{', '"':
			g, err := readGroup(expr[i:], pos+i)
			if err != nil {
				return Undefined, err
			}
			i = i + len(g) - 1
		}
	}
	return comp(left, op, expr[s:], pos+s, steps, opts)
}

func logical(left Value, op byte, expr string, pos, steps int, opts *Options,
) (Value, error) {
	expr, pos = trim(expr, pos)
	if len(expr) == 0 {
		return Undefined, errSyntax(pos)
	}
	// parse comps of expression
	right, err := evalAuto(stepComps, expr, pos, steps, opts)
	if err != nil {
		return Undefined, err
	}
	switch op {
	case '&':
		return left.and(right, pos, opts)
	case '|':
		return left.or(right, pos, opts)
	default:
		return right, nil
	}
}

func evalLogicals(expr string, pos, steps int, opts *Options) (Value, error) {
	var err error
	var s int
	var left Value
	var op byte
	for i := 0; i < len(expr); i++ {
		switch expr[i] {
		case '&', '|':
			if i == len(expr)-1 || expr[i+1] != expr[i] {
				return Undefined, errSyntax(pos + i)
			}
			left, err = logical(left, op, expr[s:i], pos+s, steps, opts)
			if err != nil {
				return Undefined, err
			}
			op = expr[i]
			i++
			s = i + 1
		case '(', '[', '{', '"':
			g, err := readGroup(expr[i:], pos+i)
			if err != nil {
				return Undefined, err
			}
			i = i + len(g) - 1
		}
	}
	return logical(left, op, expr[s:], pos+s, steps, opts)
}

func evalAuto(step int, expr string, pos, steps int, opts *Options,
) (Value, error) {
	switch step {
	case stepLogicals:
		if (steps & stepLogicals) == stepLogicals {
			return evalLogicals(expr, pos, steps, opts)
		}
		fallthrough
	case stepComps:
		if (steps & stepComps) == stepComps {
			return evalComps(expr, pos, steps, opts)
		}
		fallthrough
	case stepSums:
		if (steps & stepSums) == stepSums {
			return evalSums(expr, pos, steps, opts)
		}
		fallthrough
	case stepFacts:
		if (steps & stepFacts) == stepFacts {
			return evalFacts(expr, pos, steps, opts)
		}
		fallthrough
	default:
		return evalAtom(expr, pos, steps, opts)
	}

	// return Undefined, errInternal(
	// fmt.Errorf("unsupported step '%d'", step), pos)
}

func evalExpr(expr string, pos, steps int, opts *Options) (Value, error) {
	// logicals >> comps >> sums >> facts >> atoms
	return evalAuto(stepLogicals, expr, 0, steps, opts)
}

type Extender interface {
	// Eval allows for custom evaluation of an expression.
	Eval(expr string, udata any) (Value, error)
	// Op allows for custom evaluation of an expression.
	Op(op Op, a, b Value, udata any) (Value, error)
}

// Options for Eval
type Options struct {
	UserData any
	Extender Extender
}

// NewExtender is a convenience function for creating a simple extender using
// the provided eval and op functions.
func NewExtender(
	eval func(expr string, udata any) (Value, error),
	op func(op Op, a, b Value, udata any) (Value, error),
) Extender {
	if eval == nil {
		eval = func(expr string, udata any) (Value, error) {
			return Undefined, nil
		}
	}
	if op == nil {
		op = func(op Op, a, b Value, udata any) (Value, error) {
			return Undefined, nil
		}
	}
	return &simpleExtender{eval, op}
}

type simpleExtender struct {
	eval func(expr string, udata any) (Value, error)
	op   func(op Op, a, b Value, udata any) (Value, error)
}

func (e *simpleExtender) Eval(expr string, udata any) (Value, error) {
	return e.eval(expr, udata)
}

func (e *simpleExtender) Op(op Op, a, b Value, udata any) (Value, error) {
	return e.op(op, a, b, udata)
}

const (
	stepLogicals = 1
	stepComps    = 2
	stepSums     = 4
	stepFacts    = 8
)

var opSteps = [256]byte{
	'&': stepLogicals,
	'|': stepLogicals,

	'+': stepSums,
	'-': stepSums,

	'<': stepComps,
	'>': stepComps,
	'=': stepComps,
	'!': stepComps,

	'*': stepFacts,
	'/': stepFacts,
	'%': stepFacts,
}

// Eval evaluates an expression and returns the Result.
func Eval(expr string, opts *Options) (Value, error) {
	var pos int
	expr, pos = trim(expr, 0)
	if len(expr) == 0 {
		return Undefined, nil
	}
	var steps int
	for i := 0; i < len(expr); i++ {
		steps |= int(opSteps[expr[i]])
	}
	r, err := evalExpr(expr, pos, steps, opts)
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
	// expects that the lead character is a '[' or '{' or '(' or '"'
	// squash the value, ignoring all nested arrays and objects.
	var i, depth int
	if data[0] != '"' {
		i, depth = 1, 1
	}
	for ; i < len(data); i++ {
		if data[i] >= '"' && data[i] <= '}' {
			switch data[i] {
			case '"':
				i++
				s2 := i
				for ; i < len(data); i++ {
					if data[i] > '\\' {
						continue
					}
					if data[i] == '"' {
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
	}
	return data, false
}

var space = [256]uint8{'\t': 1, '\n': 1, '\v': 1, '\f': 1, '\r': 1, ' ': 1}

func isspace(c byte) bool {
	switch c {
	case '\t', '\n', '\v', '\f', '\r', ' ':
		return true
	}
	return false
}

// trim a simple ascii string along with position counting.
// This is a tad bit faster than strings.TrimSpace.
func trim(s string, pos int) (string, int) {
	for len(s) > 0 && space[s[0]] == 1 {
		s = s[1:]
		pos++
	}
	for len(s) > 0 && space[s[len(s)-1]] == 1 {
		s = s[:len(s)-1]
	}
	return s, pos
}
