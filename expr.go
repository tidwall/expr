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
	"unsafe"

	"github.com/tidwall/conv"
)

//lint:file-ignore ST1005 Errors allow for capitalization to match Javascript.

func errUndefined(ident string, chain bool) error {
	var err error
	if chain {
		err = fmt.Errorf("Uncaught TypeError: "+
			"Cannot read properties of undefined (reading '%s')", ident)
	} else {
		err = fmt.Errorf("ReferenceError: %s is not defined", ident)
	}
	return &errEval{err: err, udef: true}
}

func errOperator(err error) error {
	return &errEval{
		err: fmt.Errorf("OperatorError: %w", err),
	}
}

func errReference(err error) error {
	return &errEval{
		err: fmt.Errorf("ReferenceError: %w", err),
	}
}

func errCall(err error) error {
	return &errEval{
		err: fmt.Errorf("CallError: %w", err),
	}
}

func errSyntax() error {
	return &errEval{
		err: errors.New("SyntaxError"),
	}
}

// ErrStop is used to stop the EvalForEach and ForEachValue
var ErrStop = errors.New("stop")

type errEval struct {
	err  error
	udef bool
}

func (err *errEval) Error() string {
	return err.err.Error()
}

// CharPosOfErr returns the character position of where the error occured in
// the Eval function, or -1 if unknown
func CharPosOfErr(err error) int {
	return -1
}

var (
	Undefined = Value{kind: undefKind}
	Null      = Value{kind: nullKind}
)

// Op is an operator for Custom values used for the Options.Op function.
type Op string

const (
	OpAdd    Op = "+"
	OpSub    Op = "-"
	OpMul    Op = "*"
	OpDiv    Op = "/"
	OpMod    Op = "%"
	OpLt     Op = "<"
	OpEq     Op = "=="
	OpStEq   Op = "==="
	OpAnd    Op = "&&"
	OpOr     Op = "||"
	OpBitOr  Op = "|"
	OpBitXor Op = "^"
	OpBitAnd Op = "&"
	OpCoal   Op = "??"
)

func (op Op) String() string {
	return string(op)
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

func parseFloat(s string) (n float64, ok bool) {
	var i int
	var sign bool
	if len(s) > 15 {
		// Large numeric strings should be put through the built in converter.
		goto notInt
	}
	if len(s) > 0 && s[0] == '-' {
		sign = true
		i++
	}
	if i == len(s) {
		goto notInt
	}
	for ; i < len(s); i++ {
		if s[i] >= '0' && s[i] <= '9' {
			n = n*10 + float64(s[i]-'0')
		} else {
			goto notInt
		}
	}
	if sign {
		return n * -1, true
	}
	return n, true
notInt:
	f, err := strconv.ParseFloat(s, 64)
	return f, err == nil
}

func evalAtom(expr string, ctx *evalContext) (Value, error) {
	expr = trim(expr)
	if len(expr) == 0 {
		return Undefined, errSyntax()
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
				return Undefined, errSyntax()
			}
			return Float64(float64(x)), nil
		}
		fallthrough
	case '-', '.', '1', '2', '3', '4', '5', '6', '7', '8', '9':
		if len(expr) > 3 && strings.HasSuffix(expr, "64") {
			if expr[len(expr)-3] == 'u' {
				x, err := strconv.ParseUint(expr[:len(expr)-3], 10, 64)
				if err != nil {
					return Undefined, errSyntax()
				}
				return Uint64(x), nil
			}
			if expr[len(expr)-3] == 'i' {
				x, err := strconv.ParseInt(expr[:len(expr)-3], 10, 64)
				if err != nil {
					return Undefined, errSyntax()
				}
				return Int64(x), nil
			}
		}
		x, ok := parseFloat(expr)
		if !ok {
			return Undefined, errSyntax()
		}
		return Float64(x), nil
	case '"', '\'':
		var s string
		var ok bool
		s, raw, ok := parseString(expr)
		if !ok {
			return Undefined, errSyntax()
		}
		left = String(s)
		leftReady = true
		expr = expr[len(raw):]
	case '(', '{', '[':
		g, err := readGroup(expr)
		if err != nil {
			return Undefined, err
		}
		if g[0] == '(' {
			// paren groups can be evaluated and used as the leading value.
			left, err = evalExpr(g[1:len(g)-1], ctx)
			if err != nil {
				return Undefined, err
			}
			leftReady = true
			expr = expr[len(g):]
		} else if g[0] == '[' {
			left, err = multiExprsToArray(g[1:len(g)-1], ctx)
			if err != nil {
				return Undefined, err
			}
			leftReady = true
			expr = expr[len(g):]
		} else {
			// '{' not currently allowed as a leading value
			// Perhaps in the future.
			return Undefined, errSyntax()
		}
	}

	var leftIdent string

	if !leftReady {
		// probably a chainable identifier
		ident, ok := readIdent(expr)
		if !ok {
			return Undefined, errSyntax()
		}
		switch ident {
		case "new", "typeof", "void", "await", "in", "instanceof", "yield":
			return Undefined, errSyntax()
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
			left, err = getRefValue(false, Undefined, ident, false,
				ctx.base)
			if err != nil {
				return Undefined, err
			}
		}
		leftReady = true
		expr = expr[len(ident):]
		leftIdent = ident
	}

	var leftLeft Value
	var hasLeftLeft bool

	// read each chained component
	optChain := false
	for {
		// There are more components to read
		expr = trim(expr)
		if len(expr) == 0 {
			break
		}
		switch expr[0] {
		case '?':
			// Optional chaining
			if len(expr) == 1 || expr[1] != '.' {
				return Undefined, errSyntax()
			}
			expr = expr[1:]
			optChain = true
			fallthrough
		case '.':
			// Member Access
			expr = expr[1:]
			expr = trim(expr)
			ident, ok := readIdent(expr)
			if !ok {
				return Undefined, errSyntax()
			}
			val, err := getRefValue(true, left, ident, optChain, ctx.base)
			if err != nil {
				return Undefined, err
			}
			leftLeft = left
			hasLeftLeft = true
			left = val
			expr = expr[len(ident):]
			leftIdent = ident
		case '(', '[':
			g, err := readGroup(expr)
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
				if ctx.base != nil && ctx.base.Extender != nil {
					var info CallInfo
					info.Chain = hasLeftLeft
					info.Value = leftLeft
					info.Ident = left.funcIdent()
					args, err := multiExprsToArray(g[1:len(g)-1], ctx)
					if err != nil {
						return Undefined, err
					}
					info.Args = args
					val, err = ctx.base.Extender.Call(info, ctx.base)
					if err != nil {
						return Undefined, errCall(err)
					}
				}
				leftLeft = left
				hasLeftLeft = true
				left = val
			} else {
				// Computed Member Access
				last, err := evalExpr(g[1:len(g)-1], ctx)
				if err != nil {
					return Undefined, err
				}
				ident := last.String()
				val, err := getRefValue(true, left, ident, optChain,
					ctx.base)
				if err != nil {
					return Undefined, err
				}
				leftLeft = left
				hasLeftLeft = true
				left = val
			}
			expr = expr[len(g):]
		default:
			return Undefined, errSyntax()
		}
	}
	return left, nil
}

func multiExprsToArray(expr string, ctx *evalContext) (Value, error) {
	var arr []Value
	_, err := EvalForEach(expr, func(value Value) error {
		arr = append(arr, value)
		return nil
	}, ctx.base)
	if err != nil {
		return Undefined, err
	}
	return Array(arr), nil
}

func getRefValue(chain bool, left Value, ident string, optChain bool,
	ctx *Context,
) (Value, error) {
	val, err := func() (Value, error) {
		if ctx == nil || ctx.Extender == nil {
			return Undefined, errUndefined(ident, chain)
		}
		info := RefInfo{Chain: chain, Value: left, Ident: ident}
		val, err := ctx.Extender.Ref(info, ctx)
		if err != nil {
			return Undefined, errReference(err)
		}
		if val.kind == undefKind && left.kind == undefKind {
			return Undefined, errUndefined(ident, chain)
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

func appendRune(dst []byte, r rune) []byte {
	// provide enough space to encode the largest utf8 possible
	dst = append(dst, 0, 0, 0, 0, 0, 0, 0, 0)
	n := utf8.EncodeRune(dst[len(dst)-8:], r)
	return dst[:len(dst)-8+n]
}

func fact(left Value, op byte, expr string, ctx *evalContext,
) (Value, error) {
	expr = trim(expr)
	if len(expr) == 0 {
		return Undefined, errSyntax()
	}
	right, err := evalAtom(expr, ctx)
	if err != nil {
		return Undefined, err
	}
	switch op {
	case '*':
		return mul(left, right, ctx)
	case '/':
		return div(left, right, ctx)
	case '%':
		return mod(left, right, ctx)
	default:
		return right, nil
	}
}

func evalFacts(expr string, ctx *evalContext) (Value, error) {
	var err error
	var s int
	var left Value
	var op byte
	for i := 0; i < len(expr); i++ {
		switch expr[i] {
		case '*', '/', '%':
			left, err = fact(left, op, expr[s:i], ctx)
			if err != nil {
				return Undefined, err
			}
			op = expr[i]
			s = i + 1
		case '(', '[', '{', '"', '\'':
			g, err := readGroup(expr[i:])
			if err != nil {
				return Undefined, err
			}
			i = i + len(g) - 1
		}
	}
	return fact(left, op, expr[s:], ctx)
}

func sum(left Value, op byte, expr string, neg, end bool,
	ctx *evalContext,
) (Value, error) {
	expr = trim(expr)
	if len(expr) == 0 {
		return Undefined, errSyntax()
	}
	// parse factors of expression
	right, err := evalAuto(stepSums<<1, expr, ctx)
	if err != nil {
		return Undefined, err
	}
	if neg {
		// make right negative
		right, err = mul(right, Float64(-1), ctx)
		if err != nil {
			return Undefined, err
		}
	}
	switch op {
	case '+':
		return add(left, right, ctx)
	case '-':
		return sub(left, right, ctx)
	default:
		return right, nil
	}
}

func evalSums(expr string, ctx *evalContext) (Value, error) {
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
					return Undefined, errSyntax()
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
			left, err = sum(left, op, expr[s:i], neg, false, ctx)
			if err != nil {
				return Undefined, err
			}
			op = expr[i]
			s = i + 1
			fill = false
			neg = false
		case '(', '[', '{', '"', '\'':
			g, err := readGroup(expr[i:])
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
	return sum(left, op, expr[s:], neg, true, ctx)
}

func comp(left Value, op byte, expr string, ctx *evalContext,
) (Value, error) {
	expr = trim(expr)
	if len(expr) == 0 {
		return Undefined, errSyntax()
	}
	// parse next expression
	right, err := evalAuto(stepComps<<1, expr, ctx)
	if err != nil {
		return Undefined, err
	}
	switch op {
	case '<':
		return lt(left, right, ctx)
	case '<' + 32:
		return lte(left, right, ctx)
	case '>':
		return gt(left, right, ctx)
	case '>' + 32:
		return gte(left, right, ctx)
	default:
		return right, nil
	}
}

func evalComps(expr string, ctx *evalContext) (Value, error) {
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
			left, err = comp(left, op, expr[s:i], ctx)
			if err != nil {
				return Undefined, err
			}
			op = opch
			i = i + opsz - 1
			s = i + 1
		case '(', '[', '{', '"', '\'':
			g, err := readGroup(expr[i:])
			if err != nil {
				return Undefined, err
			}
			i = i + len(g) - 1
		}
	}
	return comp(left, op, expr[s:], ctx)
}

func equal(left Value, op byte, expr string, ctx *evalContext,
) (Value, error) {
	var neg bool
	var boolit bool
	expr = trim(expr)
	for {
		if len(expr) == 0 {
			return Undefined, errSyntax()
		}
		if expr[0] != '!' {
			break
		}
		neg = !neg
		boolit = true
		expr = expr[1:]
		expr = trim(expr)
	}
	// parse next expression
	right, err := evalAuto(stepEquality<<1, expr, ctx)
	if err != nil {
		return Undefined, err
	}
	if boolit {
		if right.kind != boolKind {
			right = Bool(right.Bool())
		}
		if neg {
			right = Bool(!right.asBool())
		}
	}
	switch op {
	case '=':
		return eq(left, right, ctx)
	case '!':
		return neq(left, right, ctx)
	case '=' + 32:
		return seq(left, right, ctx)
	case '!' + 32:
		return sneq(left, right, ctx)
	default:
		return right, nil
	}
}

func evalEquality(expr string, ctx *evalContext) (Value, error) {
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
					return Undefined, errSyntax()
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
			left, err = equal(left, op, expr[s:i], ctx)
			if err != nil {
				return Undefined, err
			}
			op = opch
			i = i + opsz - 1
			s = i + 1
		case '(', '[', '{', '"', '\'':
			g, err := readGroup(expr[i:])
			if err != nil {
				return Undefined, err
			}
			i = i + len(g) - 1
		}
	}
	return equal(left, op, expr[s:], ctx)
}

func bitwiseXOR(left Value, op byte, expr string, ctx *evalContext,
) (Value, error) {
	expr = trim(expr)
	if len(expr) == 0 {
		return Undefined, errSyntax()
	}
	right, err := evalAuto(stepBitwiseXOR<<1, expr, ctx)
	if err != nil {
		return Undefined, err
	}
	switch op {
	case '^':
		return xor(left, right, ctx)
	default:
		return right, nil
	}
}

func evalBitwiseXOR(expr string, ctx *evalContext) (Value, error) {
	var err error
	var s int
	var left Value
	var op byte
	for i := 0; i < len(expr); i++ {
		switch expr[i] {
		case '^':
			left, err = bitwiseXOR(left, op, expr[s:i], ctx)
			if err != nil {
				return Undefined, err
			}
			op = expr[i]
			s = i + 1
		case '(', '[', '{', '"', '\'':
			g, err := readGroup(expr[i:])
			if err != nil {
				return Undefined, err
			}
			i = i + len(g) - 1
		}
	}
	return bitwiseXOR(left, op, expr[s:], ctx)
}

func bitwiseOR(left Value, op byte, expr string, ctx *evalContext,
) (Value, error) {
	expr = trim(expr)
	if len(expr) == 0 {
		return Undefined, errSyntax()
	}
	right, err := evalAuto(stepBitwiseOR<<1, expr, ctx)
	if err != nil {
		return Undefined, err
	}
	switch op {
	case '|':
		return bor(left, right, ctx)
	default:
		return right, nil
	}
}

func evalBitwiseOR(expr string, ctx *evalContext) (Value, error) {
	var err error
	var s int
	var left Value
	var op byte
	for i := 0; i < len(expr); i++ {
		switch expr[i] {
		case '|':
			left, err = bitwiseOR(left, op, expr[s:i], ctx)
			if err != nil {
				return Undefined, err
			}
			op = expr[i]
			s = i + 1
		case '(', '[', '{', '"', '\'':
			g, err := readGroup(expr[i:])
			if err != nil {
				return Undefined, err
			}
			i = i + len(g) - 1
		}
	}
	return bitwiseOR(left, op, expr[s:], ctx)
}

func bitwiseAND(left Value, op byte, expr string, ctx *evalContext,
) (Value, error) {
	expr = trim(expr)
	if len(expr) == 0 {
		return Undefined, errSyntax()
	}
	right, err := evalAuto(stepBitwiseAND<<1, expr, ctx)
	if err != nil {
		return Undefined, err
	}
	switch op {
	case '&':
		return band(left, right, ctx)
	default:
		return right, nil
	}
}

func evalBitwiseAND(expr string, ctx *evalContext) (Value, error) {
	var err error
	var s int
	var left Value
	var op byte
	for i := 0; i < len(expr); i++ {
		switch expr[i] {
		case '&':
			left, err = bitwiseAND(left, op, expr[s:i], ctx)
			if err != nil {
				return Undefined, err
			}
			op = expr[i]
			s = i + 1
		case '(', '[', '{', '"', '\'':
			g, err := readGroup(expr[i:])
			if err != nil {
				return Undefined, err
			}
			i = i + len(g) - 1
		}
	}
	return bitwiseAND(left, op, expr[s:], ctx)
}

func logicalAND(left Value, op byte, expr string, ctx *evalContext,
) (Value, error) {
	expr = trim(expr)
	if len(expr) == 0 {
		return Undefined, errSyntax()
	}
	right, err := evalAuto(stepLogicalAND<<1, expr, ctx)
	if err != nil {
		return Undefined, err
	}
	switch op {
	case '&':
		return and(left, right, ctx)
	default:
		return right, nil
	}
}

func evalLogicalAND(expr string, ctx *evalContext) (Value, error) {
	var err error
	var s int
	var left Value
	var op byte
	for i := 0; i < len(expr); i++ {
		switch expr[i] {
		case '&':
			if i+1 == len(expr) {
				return Undefined, errSyntax()
			}
			if expr[i+1] != '&' {
				// bitwise AND
				i++
				continue
			}
			left, err = logicalAND(left, op, expr[s:i], ctx)
			if err != nil {
				return Undefined, err
			}
			op = expr[i]
			i++
			s = i + 1
		case '(', '[', '{', '"', '\'':
			g, err := readGroup(expr[i:])
			if err != nil {
				return Undefined, err
			}
			i = i + len(g) - 1
		}
	}
	return logicalAND(left, op, expr[s:], ctx)
}

func logicalOR(left Value, op byte, expr string, ctx *evalContext,
) (Value, error) {
	expr = trim(expr)
	if len(expr) == 0 {
		return Undefined, errSyntax()
	}
	right, err := evalAuto(stepLogicalOR<<1, expr, ctx)
	if err != nil {
		return Undefined, err
	}
	switch op {
	case '|':
		return or(left, right, ctx)
	case '?':
		return coalesce(left, right, ctx)
	default:
		return right, nil
	}
}

func evalLogicalOR(expr string, ctx *evalContext) (Value, error) {
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
			if i+1 == len(expr) {
				return Undefined, errSyntax()
			}
			if expr[i+1] != expr[i] {
				// bitwise OR
				i++
				continue
			}
			left, err = logicalOR(left, op, expr[s:i], ctx)
			if err != nil {
				return Undefined, err
			}
			op = expr[i]
			i++
			s = i + 1
		case '(', '[', '{', '"', '\'':
			g, err := readGroup(expr[i:])
			if err != nil {
				return Undefined, err
			}
			i = i + len(g) - 1
		}
	}
	return logicalOR(left, op, expr[s:], ctx)
}

func evalTerns(expr string, ctx *evalContext) (Value, error) {
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
				res, err := evalExpr(cond, ctx)
				if err != nil {
					return Undefined, err
				}
				if res.Bool() {
					return evalExpr(left, ctx)
				}
				return evalExpr(right, ctx)
			}
		case '(', '[', '{', '"', '\'':
			g, err := readGroup(expr[i:])
			if err != nil {
				return Undefined, err
			}
			i = i + len(g) - 1
		}
	}
	if depth == 0 {
		return evalAuto(stepTerns<<1, expr, ctx)
	}
	return Undefined, errSyntax()
}

func evalComma(expr string, ctx *evalContext) (Value, error) {
	var s int
	iter := ctx.iter
	for i := 0; i < len(expr); i++ {
		switch expr[i] {
		case ',':
			ctx.iter = nil // disable the iter
			res, err := evalAuto(stepComma<<1, expr[s:i], ctx)
			ctx.iter = iter // enable the iter
			if err != nil {
				return Undefined, err
			}
			if ctx.iter != nil {
				if err := ctx.iter(res); err != nil {
					if err == ErrStop {
						return res, nil
					}
					return Undefined, err
				}
			}
			s = i + 1
		case '(', '[', '{', '"', '\'':
			g, err := readGroup(expr[i:])
			if err != nil {
				return Undefined, err
			}
			i = i + len(g) - 1
		}
	}
	res, err := evalAuto(stepComma<<1, expr[s:], ctx)
	if err != nil {
		return Undefined, err
	}
	if ctx.iter != nil {
		if err := ctx.iter(res); err != nil {
			if err == ErrStop {
				return res, nil
			}
			return Undefined, err
		}
	}
	return res, nil
}

func evalAuto(step int, expr string, ctx *evalContext) (Value, error) {
	switch step {
	case stepComma:
		if (ctx.steps & stepComma) == stepComma {
			return evalComma(expr, ctx)
		}
		fallthrough
	case stepTerns:
		if (ctx.steps & stepTerns) == stepTerns {
			return evalTerns(expr, ctx)
		}
		fallthrough
	case stepLogicalOR:
		if (ctx.steps & stepLogicalOR) == stepLogicalOR {
			return evalLogicalOR(expr, ctx)
		}
		fallthrough
	case stepLogicalAND:
		if (ctx.steps & stepLogicalAND) == stepLogicalAND {
			return evalLogicalAND(expr, ctx)
		}
		fallthrough
	case stepBitwiseOR:
		if (ctx.steps & stepBitwiseOR) == stepBitwiseOR {
			return evalBitwiseOR(expr, ctx)
		}
		fallthrough
	case stepBitwiseXOR:
		if (ctx.steps & stepBitwiseXOR) == stepBitwiseXOR {
			return evalBitwiseXOR(expr, ctx)
		}
		fallthrough
	case stepBitwiseAND:
		if (ctx.steps & stepBitwiseAND) == stepBitwiseAND {
			return evalBitwiseAND(expr, ctx)
		}
		fallthrough
	case stepEquality:
		if (ctx.steps & stepEquality) == stepEquality {
			return evalEquality(expr, ctx)
		}
		fallthrough
	case stepComps:
		if (ctx.steps & stepComps) == stepComps {
			return evalComps(expr, ctx)
		}
		fallthrough
	case stepSums:
		if (ctx.steps & stepSums) == stepSums {
			return evalSums(expr, ctx)
		}
		fallthrough
	case stepFacts:
		if (ctx.steps & stepFacts) == stepFacts {
			return evalFacts(expr, ctx)
		}
		fallthrough
	default:
		return evalAtom(expr, ctx)
	}
}

func evalExpr(expr string, ctx *evalContext) (Value, error) {
	return evalAuto(stepComma, expr, ctx)
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
	Args  Value
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
	NoCase   bool // Disable case insensitive string comparisons
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
	stepLogicalOR              //  4: Logical OR (||) Nullish coalescing operator (??)
	stepLogicalAND             //  5: Logical AND (&&)
	stepBitwiseOR              //  6: Bitwise OR (|)
	stepBitwiseXOR             //  7: Bitwise XOR (^)
	stepBitwiseAND             //  8: Bitwise AND (&)
	stepEquality               //  9: Equality (==) (!=)
	stepComps                  // 10: Comparison (<) (<=) (>) (>=)
	stepSums                   // 12: Summation (-) (+)
	stepFacts                  // 13: Factors (*) (/)
)

// all step tokens
var opSteps = [256]uint16{
	',': stepComma,                       // ','
	'?': stepTerns | stepLogicalOR,       // '?:' '??'
	':': stepTerns,                       // '?:'
	'|': stepLogicalOR | stepBitwiseOR,   // '||' '|'
	'&': stepLogicalAND | stepBitwiseAND, // '&&' '&'
	'^': stepBitwiseXOR,                  // '^'
	'=': stepComps | stepEquality,        // '==' '<=' '>='
	'!': stepEquality,                    // '!' '!='
	'<': stepComps,                       // '<' '<='
	'>': stepComps,                       // '>' '>='
	'+': stepSums,                        // '+'
	'-': stepSums,                        // '-'
	'*': stepFacts,                       // '*'
	'/': stepFacts,                       // '/'
	'%': stepFacts,                       // '%'
}

type evalContext struct {
	expr  string                  // original expression
	steps int                     // all possible steps
	iter  func(value Value) error // iterator, if any
	base  *Context                // user context
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
	expr = trim(expr)
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
	ectx := evalContext{expr, steps, iter, ctx}
	r, err := evalExpr(expr, &ectx)
	if err != nil {
		return Undefined, err
	}
	return r, nil
}

func readGroup(data string) (string, error) {
	g, ok := squash(data)
	if !ok {
		return "", errSyntax()
	}
	if len(g) < 2 || g[len(g)-1] != closech(data[0]) {
		return "", errSyntax()
	}
	return g, nil
}

func squash(data string) (string, bool) {
	// expects that the lead character is
	//   '[' or '{' or '(' or '"' or '\''
	// squash the value, ignoring all nested arrays and objects.
	var i, depth int
	switch data[0] {
	case '"', '\'':
	default:
		i, depth = 1, 1
	}
	for ; i < len(data); i++ {
		if data[i] < '"' || data[i] > '}' {
			continue
		}
		switch data[i] {
		case '"', '\'':
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
func trim(s string) string {
	for len(s) > 0 && isspace(s[0]) {
		s = s[1:]
	}
	for len(s) > 0 && isspace(s[len(s)-1]) {
		s = s[:len(s)-1]
	}
	return s
}

////////////////////////////////////////////////////////////////
// Value
////////////////////////////////////////////////////////////////

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
	arrKind               // array kind
)

// Value represents is the return value of Eval.
type Value struct {
	kind  kind           // kind MUST match correct values below (or segfault)
	bits  uint64         // all number types and string length
	rtype unsafe.Pointer // interface type pointer
	data  unsafe.Pointer // interface data pointer and string data pointer
}

// IsUndefined returns true if the value is 'undefined'
func (a Value) IsUndefined() bool {
	return a.kind == undefKind
}

// IsNull returns true if the value is 'null'
func (a Value) IsNull() bool {
	return a.kind == nullKind
}

///////////////////////////////////////////
// Bool
///////////////////////////////////////////

// Bool returns a bool value.
func Bool(t bool) Value {
	v := Value{}
	v.kind = boolKind
	*(*bool)(unsafe.Pointer(&v.bits)) = t
	return v
}

// asBool converts the raw value bits to a bool
// UNSAFE: The Value kind MUST BE a boolean!
func (a Value) asBool() bool {
	return *(*bool)(unsafe.Pointer(&a.bits))
}

// Bool returns a boolean representation.
func (a Value) Bool() bool {
	if a.kind == boolKind {
		return a.asBool()
	}
	return a.toBool()
}

//go:noinline
func (a Value) toBool() bool {
	switch a.kind {
	case undefKind:
		return false
	case nullKind:
		return false
	case boolKind:
		return a.asBool()
	case floatKind:
		return conv.Ftot(a.asFloat64())
	case intKind:
		return conv.Itot(a.asInt64())
	case uintKind:
		return conv.Utot(a.asUint64())
	case strKind:
		return conv.Atot(a.asString())
	case objKind:
		return conv.Vtot(a.asObject())
	}
	// fallback to using a floating point converstion
	return conv.Ftot(a.toFloat64())
}

///////////////////////////////////////////
// Float64
///////////////////////////////////////////

// Float64 returns an int64 value.
func Float64(x float64) Value {
	return Value{
		kind: floatKind,
		bits: math.Float64bits(x),
	}
}

func (a Value) asFloat64() float64 {
	return math.Float64frombits(a.bits)
}

// Float64 returns s float64 representation.
func (a Value) Float64() float64 {
	if a.kind == floatKind {
		return a.asFloat64()
	}
	return a.toFloat64()
}
func (a Value) toFloat64() float64 {
	switch a.kind {
	case nullKind:
		return 0
	case boolKind:
		return conv.Ttof(a.asBool())
	case floatKind:
		return a.asFloat64()
	case intKind:
		return conv.Itof(a.asInt64())
	case uintKind:
		return conv.Utof(a.asUint64())
	case strKind:
		return conv.Atof(a.asString())
	case objKind:
		return conv.Vtof(a.asObject())
	case arrKind:
		return conv.Atof(a.toString())
	}
	// everything else NaN
	return math.NaN()
}

///////////////////////////////////////////
// Int64
///////////////////////////////////////////

// Int64 returns an int64 value.
func Int64(x int64) Value {
	return Value{
		kind: intKind,
		bits: uint64(x),
	}
}
func (a Value) asInt64() int64 {
	return int64(a.bits)
}

// Int64 returns an int64 representation.
func (a Value) Int64() int64 {
	if a.kind == intKind {
		return a.asInt64()
	}
	return a.toInt64()
}
func (a Value) toInt64() int64 {
	switch a.kind {
	case boolKind:
		return conv.Ttoi(a.asBool())
	case floatKind:
		return conv.Ftoi(a.asFloat64())
	case intKind:
		return a.asInt64()
	case uintKind:
		return conv.Utoi(a.asUint64())
	case strKind:
		return conv.Atoi(a.asString())
	case objKind:
		return conv.Vtoi(a.asObject())
	case arrKind:
		return conv.Atoi(a.toString())
	}
	// everything else zero
	return 0
}

///////////////////////////////////////////
// Uint64
///////////////////////////////////////////

// Uint64 returns a uint64 value.
func Uint64(x uint64) Value {
	return Value{
		kind: uintKind,
		bits: x,
	}
}
func (a Value) asUint64() uint64 {
	return a.bits
}

// Uint64 returns a uint64 representation.
func (a Value) Uint64() uint64 {
	if a.kind == uintKind {
		return a.asUint64()
	}
	return a.toUint64()
}
func (a Value) toUint64() uint64 {
	switch a.kind {
	case boolKind:
		return conv.Ttou(a.asBool())
	case floatKind:
		return conv.Ftou(a.asFloat64())
	case intKind:
		return conv.Itou(a.asInt64())
	case uintKind:
		return a.asUint64()
	case strKind:
		return conv.Atou(a.asString())
	case objKind:
		return conv.Vtou(a.asObject())
	case arrKind:
		return conv.Atou(a.toString())
	}
	// everything else zero
	return 0
}

///////////////////////////////////////////
// String
///////////////////////////////////////////

type sface struct {
	ptr unsafe.Pointer
	len int
}

// String returns a string value.
func String(s string) Value {
	return Value{
		kind: strKind,
		bits: uint64((*sface)(unsafe.Pointer(&s)).len),
		data: (*sface)(unsafe.Pointer(&s)).ptr,
	}
}

// asString converts value to string. UNSAFE: Must be strKind!
func (a Value) asString() string {
	return *(*string)(unsafe.Pointer(&sface{a.data, int(a.bits)}))
}

// String returns a string representation.
func (a Value) String() string {
	if a.kind == strKind {
		return a.asString()
	}
	return a.toString()
}

func (a Value) toString() string {
	switch a.kind {
	case undefKind:
		return "undefined"
	case nullKind:
		return "null"
	case boolKind:
		return conv.Ttoa(a.asBool())
	case floatKind:
		return conv.Ftoa(a.asFloat64())
	case intKind:
		return conv.Itoa(a.asInt64())
	case uintKind:
		return conv.Utoa(a.asUint64())
	case strKind:
		return a.asString()
	case funcKind:
		return "[Function: " + a.asString() + "]"
	case objKind:
		return conv.Vtoa(a.asObject())
	case arrKind:
		vals := a.asArray()
		var str []byte
		for i := 0; i < len(vals); i++ {
			if i > 0 {
				str = append(str, ',')
			}
			str = append(str, vals[i].String()...)
		}
		return string(str)
	}
	// everything else undefined
	return "undefined"
}

///////////////////////////////////////////
// Object
///////////////////////////////////////////

type iface struct {
	rtype unsafe.Pointer
	data  unsafe.Pointer
}

// Object returns a custom user-defined object.
func Object(o interface{}) Value {
	return Value{
		kind:  objKind,
		rtype: (*iface)(unsafe.Pointer(&o)).rtype,
		data:  (*iface)(unsafe.Pointer(&o)).data,
	}
}

// asObject converts value to interface{}. UNSAFE: Must be objKind!
func (a Value) asObject() interface{} {
	return *(*interface{})(unsafe.Pointer(&iface{a.rtype, a.data}))
}

// Object returns the native Go representation.
func (a Value) Object() any {
	switch a.kind {
	case boolKind:
		return conv.Ttov(a.asBool())
	case floatKind:
		return conv.Ftov(a.asFloat64())
	case intKind:
		return conv.Itov(a.asInt64())
	case uintKind:
		return conv.Utov(a.asUint64())
	case strKind:
		return conv.Atov(a.asString())
	case objKind:
		return a.asObject()
	case arrKind:
		return a.asArray()
	}
	// everything else nil
	return nil
}

// Object returns the native Go representation.
// Same as Value.Object()
func (a Value) Value() any {
	return a.Object()
}

///////////////////////////////////////////
// Array
///////////////////////////////////////////

type aface struct {
	ptr unsafe.Pointer
	len int
	cap int
}

func Array(values []Value) Value {
	return Value{
		kind: arrKind,
		bits: uint64((*aface)(unsafe.Pointer(&values)).len),
		data: (*aface)(unsafe.Pointer(&values)).ptr,
	}
}

func (a Value) asArray() []Value {
	return *(*[]Value)(unsafe.Pointer(&aface{
		ptr: a.data,
		len: int(a.bits),
		cap: int(a.bits),
	}))
}

func (a Value) Array() []Value {
	if a.kind == arrKind {
		return a.asArray()
	}
	return a.toArray()
}

func (a Value) toArray() []Value {
	if a.kind == arrKind {
		return a.asArray()
	}
	return nil
}

// At returns a value from an array at index.
// Returns 'undefined' if the value is not an array or the index is
// outside of the bounds.
func (a Value) At(i int) Value {
	if a.kind == arrKind {
		arr := a.asArray()
		if i >= 0 && i < len(arr) {
			return arr[i]
		}
	}
	return Undefined
}

// Len return the length of a String or Array.
// Returns zero other types.
func (a Value) Len() int {
	if a.kind == strKind {
		return len(a.asString())
	}
	if a.kind == arrKind {
		return len(a.asArray())
	}
	return 0
}

// IsArray returns true if the value is an 'Array'
func (a Value) IsArray() bool {
	return a.kind == arrKind
}

///////////////////////////////////////////
// Function
///////////////////////////////////////////

// Function
func Function(name string) Value {
	v := String(name)
	v.kind = funcKind
	return v
}

func (a Value) funcIdent() string {
	if a.kind == funcKind {
		return a.asString()
	}
	return ""
}

// Number returns a float64 value.
func Number(x float64) Value {
	return Float64(x)
}

// Number returns s float64 representation.
func (a Value) Number() float64 {
	return a.Float64()
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

func doOp(op Op, a, b Value, ctx *evalContext) (Value, error) {
	if ctx.base != nil && ctx.base.Extender != nil {
		info := OpInfo{Left: a, Op: op, Right: b}
		v, err := ctx.base.Extender.Op(info, ctx.base)
		if err == nil {
			return v, nil
		}
		return Undefined, errOperator(err)
	}
	return Undefined, errOperator(errors.New("undefined "))
}

func add(a, b Value, ctx *evalContext) (Value, error) {
	if a.kind == objKind || b.kind == objKind {
		return doOp(OpAdd, a, b, ctx)
	}
	if a.kind == b.kind {
		switch a.kind {
		case floatKind:
			return Float64(a.asFloat64() + b.asFloat64()), nil
		case intKind:
			return Int64(a.asInt64() + b.asInt64()), nil
		case uintKind:
			return Uint64(a.asUint64() + b.asUint64()), nil
		case strKind:
			return String(a.asString() + b.asString()), nil
		case boolKind, undefKind, nullKind:
			return Float64(a.Float64() + b.Float64()), nil
		}
	} else if a.isnum() && b.isnum() {
		return Float64(a.Float64() + b.Float64()), nil
	}
	return String(a.String() + b.String()), nil
}

func (a Value) isnum() bool {
	switch a.kind {
	case floatKind, intKind, uintKind, boolKind, nullKind, undefKind:
		return true
	}
	return false
}

func bor(a, b Value, ctx *evalContext) (Value, error) {
	if a.kind == objKind || b.kind == objKind {
		return doOp(OpBitOr, a, b, ctx)
	}
	if a.kind == b.kind {
		switch a.kind {
		case intKind:
			return Int64(a.asInt64() | b.asInt64()), nil
		case uintKind:
			return Uint64(a.asUint64() | b.asUint64()), nil
		}
	}
	return Float64(conv.Itof(a.Int64() | b.Int64())), nil
}
func band(a, b Value, ctx *evalContext) (Value, error) {
	if a.kind == objKind || b.kind == objKind {
		return doOp(OpBitAnd, a, b, ctx)
	}
	if a.kind == b.kind {
		switch a.kind {
		case intKind:
			return Int64(a.asInt64() & b.asInt64()), nil
		case uintKind:
			return Uint64(a.asUint64() & b.asUint64()), nil
		}
	}
	return Float64(conv.Itof(a.Int64() & b.Int64())), nil
}
func xor(a, b Value, ctx *evalContext) (Value, error) {
	if a.kind == objKind || b.kind == objKind {
		return doOp(OpBitXor, a, b, ctx)
	}
	if a.kind == b.kind {
		switch a.kind {
		case intKind:
			return Int64(a.asInt64() ^ b.asInt64()), nil
		case uintKind:
			return Uint64(a.asUint64() ^ b.asUint64()), nil
		}
	}
	return Float64(conv.Itof(a.Int64() ^ b.Int64())), nil
}

func sub(a, b Value, ctx *evalContext) (Value, error) {
	if a.kind == objKind || b.kind == objKind {
		return doOp(OpSub, a, b, ctx)
	}
	if a.kind == b.kind {
		switch a.kind {
		case floatKind:
			return Float64(a.asFloat64() - b.asFloat64()), nil
		case intKind:
			return Int64(a.asInt64() - b.asInt64()), nil
		case uintKind:
			return Uint64(a.asUint64() - b.asUint64()), nil
		}
	}
	return Float64(a.Float64() - b.Float64()), nil
}

func mul(a, b Value, ctx *evalContext) (Value, error) {
	if a.kind == objKind || b.kind == objKind {
		return doOp(OpMul, a, b, ctx)
	}
	if a.kind == b.kind {
		switch a.kind {
		case floatKind:
			return Float64(a.asFloat64() * b.asFloat64()), nil
		case intKind:
			return Int64(a.asInt64() * b.asInt64()), nil
		case uintKind:
			return Uint64(a.asUint64() * b.asUint64()), nil
		}
	}
	return Float64(a.Float64() * b.Float64()), nil
}

func div(a, b Value, ctx *evalContext) (Value, error) {
	if a.kind == objKind || b.kind == objKind {
		return doOp(OpDiv, a, b, ctx)
	}
	if a.kind == b.kind {
		switch a.kind {
		case floatKind:
			return Float64(a.asFloat64() / b.asFloat64()), nil
		case intKind:
			if b.asInt64() == 0 {
				return Float64(math.NaN()), nil
			}
			return Int64(a.asInt64() / b.asInt64()), nil
		case uintKind:
			if b.asUint64() == 0 {
				return Float64(math.NaN()), nil
			}
			return Uint64(a.asUint64() / b.asUint64()), nil
		}
	}
	return Float64(a.Float64() / b.Float64()), nil
}
func mod(a, b Value, ctx *evalContext) (Value, error) {
	if a.kind == objKind || b.kind == objKind {
		return doOp(OpMod, a, b, ctx)
	}
	if a.kind == b.kind {
		switch a.kind {
		case intKind:
			if b.asInt64() == 0 {
				return Float64(math.NaN()), nil
			}
			return Int64(a.asInt64() % b.asInt64()), nil
		case uintKind:
			if b.asUint64() == 0 {
				return Float64(math.NaN()), nil
			}
			return Uint64(a.asUint64() % b.asUint64()), nil
		}
	}
	return Float64(math.Mod(a.Float64(), b.Float64())), nil
}

func tolower(c byte) byte {
	if c >= 'A' && c <= 'Z' {
		return c + 32
	}
	return c
}

func stringLessInsensitive(a, b string) bool {
	for i := 0; i < len(a) && i < len(b); i++ {
		ca, cb := tolower(a[i]), tolower(b[i])
		if ca < cb {
			return true
		} else if ca > cb {
			return false
		}
	}
	return len(a) < len(b)
}

func stringEqualInsensitive(a, b string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := 0; i < len(a); i++ {
		if tolower(a[i]) != tolower(b[i]) {
			return false
		}
	}
	return true
}

func lt(a, b Value, ctx *evalContext) (Value, error) {
	if a.kind == objKind || b.kind == objKind {
		return doOp(OpLt, a, b, ctx)
	}
	if a.kind == b.kind {
		switch a.kind {
		case floatKind:
			return Bool(a.asFloat64() < b.asFloat64()), nil
		case intKind:
			return Bool(a.asInt64() < b.asInt64()), nil
		case uintKind:
			return Bool(a.asUint64() < b.asUint64()), nil
		case strKind:
			var less bool
			if ctx != nil && ctx.base != nil && ctx.base.NoCase {
				less = stringLessInsensitive(a.asString(), b.asString())
			} else {
				less = a.asString() < b.asString()
			}
			return Bool(less), nil
		}
	}
	return Bool(a.Float64() < b.Float64()), nil
}

func lte(a, b Value, ctx *evalContext) (Value, error) {
	t, err := lt(a, b, ctx)
	if err != nil {
		return Undefined, err
	}
	if t.Bool() {
		return t, nil
	}
	return eq(a, b, ctx)
}

func gt(a, b Value, ctx *evalContext) (Value, error) {
	return lt(b, a, ctx)
}

func gte(a, b Value, ctx *evalContext) (Value, error) {
	t, err := gt(a, b, ctx)
	if err != nil {
		return Undefined, err
	}
	if t.Bool() {
		return t, nil
	}
	return eq(a, b, ctx)
}

func eq(a, b Value, ctx *evalContext) (Value, error) {
	if a.kind == objKind || b.kind == objKind {
		return doOp(OpEq, a, b, ctx)
	}
	if a.kind == b.kind {
		switch a.kind {
		case floatKind:
			return Bool(a.asFloat64() == b.asFloat64()), nil
		case intKind:
			return Bool(a.asInt64() == b.asInt64()), nil
		case uintKind:
			return Bool(a.asUint64() == b.asUint64()), nil
		case strKind:
			var less bool
			if ctx != nil && ctx.base != nil && ctx.base.NoCase {
				less = stringEqualInsensitive(a.asString(), b.asString())
			} else {
				less = a.asString() == b.asString()
			}
			return Bool(less), nil
		case boolKind:
			return Bool(a.asBool() == b.asBool()), nil
		case undefKind, nullKind:
			return Bool(true), nil
		}
	}
	if a.kind != b.kind && a.kind != objKind && b.kind != objKind {
		return Bool(a.Float64() == b.Float64()), nil // MARK: float equality
	}
	t, err := lt(a, b, ctx)
	if err != nil {
		return Undefined, err
	}
	if t.Bool() {
		return Bool(false), nil
	}
	t, err = lt(b, a, ctx)
	if err != nil {
		return Undefined, err
	}
	return Bool(!t.Bool()), nil
}

func seq(a, b Value, ctx *evalContext) (Value, error) {
	if a.kind == b.kind {
		return eq(a, b, ctx)
	}
	return Bool(false), nil
}

func neq(a, b Value, ctx *evalContext) (Value, error) {
	val, err := eq(a, b, ctx)
	if err != nil {
		return Undefined, err
	}
	return Bool(!val.Bool()), nil
}

func sneq(a, b Value, ctx *evalContext) (Value, error) {
	val, err := seq(a, b, ctx)
	if err != nil {
		return Undefined, err
	}
	return Bool(!val.Bool()), nil
}

func and(a, b Value, ctx *evalContext) (Value, error) {
	if a.kind == objKind || b.kind == objKind {
		return doOp(OpAnd, a, b, ctx)
	}
	return Bool(a.Bool() && b.Bool()), nil
}

func or(a, b Value, ctx *evalContext) (Value, error) {
	if a.kind == objKind || b.kind == objKind {
		return doOp(OpOr, a, b, ctx)
	}
	return Bool(a.Bool() || b.Bool()), nil
}

func coalesce(a, b Value, ctx *evalContext) (Value, error) {
	if a.kind == objKind || b.kind == objKind {
		return doOp(OpCoal, a, b, ctx)
	}
	switch a.kind {
	case undefKind, nullKind:
		return b, nil
	}
	return a, nil
}
