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
	"testing"
	"time"
)

var testTable = []string{
	(``), (`undefined`),
	(` `), (`undefined`),
	(`()`), (`SyntaxError`),
	(`"\"`), (`SyntaxError`),
	(`1`), (`1`),
	(`-1`), (`-1`),
	(`- 1`), (`-1`),
	(` - 1`), (`-1`),
	(` - -1`), (`1`),
	(`- - 1`), (`1`),
	(`- - - -1`), (`1`),
	(`- - - -1 - 2`), (`-1`),
	(`+1`), (`1`),
	(`+ 1`), (`1`),
	(` + 1`), (`1`),
	(` + +1`), (`1`),
	(` + +-1`), (`-1`),
	(` + +-+ +- -1`), (`-1`),
	(`-+-+-+-1 - 2`), (`-1`),
	(`(`), (`SyntaxError`),
	(`(1`), (`SyntaxError`),
	(`(1)`), (`1`),
	(`( 1 )`), (`1`),
	(`--1`), (`SyntaxError`),
	(`1--`), (`SyntaxError`),
	(`1++`), (`SyntaxError`),
	(`++1`), (`SyntaxError`),
	(`-+1`), (`-1`),
	(`"hello"`), (`hello`),
	(`"hel\nlo"`), ("hel\nlo"),
	(`"hi"+1`), (`hi1`),
	(`"hi"-1`), (`NaN`),
	(`1+1-0.5`), (`1.5`),
	(`2*4`), (`8`),
	(`(2*4`), (`SyntaxError`),
	(`"2*4`), (`SyntaxError`),
	(`1 > 2`), (`false`),
	(`1 > 2 || 3 > 2`), (`true`),
	(`2 > 3`), (`false`),
	(`3 > 2 || (2 > 3 && 1 < 2)`), (`true`),
	(`(1 < 2 && 3 > 2) + 10`), (`11`),
	(`999 + 777 * (888 + (0.5 + 1.5)) * (0.5 + true)`), (`1038294`),
	(`999 + 777 * (888 / 0.456) / true`), (`1514104.2631578946`),
	(`999 + 777 * (888 / 0.456) / 0`), (`Infinity`),
	(`999 + 777 * (888 / 0.456) / 0`), (`Infinity`),
	(`1.0e1`), (`10`),
	(`1.0E1`), (`10`),
	(`1.0e+1`), (`10`),
	(`1.0E+1`), (`10`),
	(`1.0e-1`), (`0.1`),
	(`1.0E-1`), (`0.1`),
	(`-1.0E-1`), (`-0.1`),
	("\"he\\\"\\b\\fllo\""), ("he\"\b\fllo"),
	(`("hello\\\t\/\r\n\t\\\"world")`), ("hello\\\t/\r\n\t\\\"world"),
	(`"hello`), ("SyntaxError"),
	(`1 | 2`), ("SyntaxError"),
	(`1 & 2`), ("SyntaxError"),
	(`(1 && 2}`), ("SyntaxError"),
	(`1 != 2`), ("true"),
	(`1 ! 2`), ("SyntaxError"),
	(`1 >= 2`), ("false"),
	(`1 == 2`), ("false"),
	(`1 = 2`), ("SyntaxError"),
	(`1 == `), ("SyntaxError"),
	(` == 1`), ("SyntaxError"),
	(`"Example emoji, KO: \ud83d\udd13, \ud83c\udfc3 OK: \u2764\ufe0f "`), (`Example emoji, KO: 🔓, 🏃 OK: ❤️ `),
	(`"Example emoji, KO: \u{d83d}\udd13, \ud83c\udfc3 OK: \u2764\ufe0f "`), (`Example emoji, KO: 🔓, 🏃 OK: ❤️ `),
	(`"Example emoji, KO: \u{d83d}\u{dd13}, \ud83c\udfc3 OK: \u2764\ufe0f "`), (`Example emoji, KO: 🔓, 🏃 OK: ❤️ `),
	(`"Example emoji, KO: \u{d83d}\u{dd13}, \u{d83c}\udfc3 OK: \u2764\ufe0f "`), (`Example emoji, KO: 🔓, 🏃 OK: ❤️ `),
	(`"Example emoji, KO: \u{d83d}\u{dd13}, \u{d83c}\u{dfc3} OK: \u2764\ufe0f "`), (`Example emoji, KO: 🔓, 🏃 OK: ❤️ `),
	(`"Example emoji, KO: \u{d83d}\u{dd13}, \u{d83c}\u{dfc3} OK: \u{2764}\ufe0f "`), (`Example emoji, KO: 🔓, 🏃 OK: ❤️ `),
	(`"Example emoji, KO: \u{d83d}\u{dd13}, \u{d83c}\u{dfc3} OK: \u{2764}\u{fe0f} "`), (`Example emoji, KO: 🔓, 🏃 OK: ❤️ `),
	(`"KO: \xffsd"`), (`KO: ÿsd`),
	(`"KO: \ud8"`), (`SyntaxError`),
	(`"KO: \zd8"`), (`KO: zd8`),
	(`"KO: ` + string(byte(0)) + `"`), (`SyntaxError`),
	(`false == true`), (`false`),
	(`false + true`), (`1`),
	(`false - true`), (`-1`),
	(`NaN + 1`), (`NaN`),
	(`NaN * 1`), (`NaN`),
	(`0.24ab31 - 1`), (`SyntaxError`),
	(`0 + {1}`), (`ReferenceError: {1} is not defined`),
	(`0 + [1]`), (`ReferenceError: [1] is not defined`),
	(`hello + 2`), (`ReferenceError: hello is not defined`),
	(`i64(-9223372036854775808)`), (`-9223372036854775808`),
	(`i64(9223372036854775807)`), (`9223372036854775807`),
	(`i64(-9223372036854775808)`), (`-9223372036854775808`),
	(`u64(18446744073709551615) - u64(18446744073709551614)`), (`1`),
	(`u64(18446744073709551614) + u64(1)`), (`18446744073709551615`),
	(`i64(-9223372036854775808) + i64(1)`), (`-9223372036854775807`),
	(`i64(9223372036854775807) - i64(1)`), (`9223372036854775806`),
	(`i64(9223372036854775807) - 1`), (`9223372036854776000`),
	(`u64(9223372036854775807) - 1`), (`9223372036854776000`),
	(`u64(1) > 0`), (`true`),
	(`u64(1) >= 0`), (`true`),
	(`u64(0) >= 0`), (`true`),
	(`i64(0) >= 0`), (`true`),
	(`i64(0) >= 0`), (`true`),
	(`i64(-1) >= 0`), (`false`),
	(`i64(-1) >= i64(0)`), (`false`),
	(`u64(1) >= u64(0)`), (`true`),
	(`u64(1) > u64(0)`), (`true`),
	(`"1" >= "2" `), (`false`),
	(`"2" >= "2" `), (`true`),
	(`"2" >= "10" `), (`true`),
	(`"1" > "2" `), (`false`),
	(`"2" > "2" `), (`false`),
	(`"2" > "10" `), (`true`),
	(`i64(2) > i64(10)`), (`false`),
	(`i64(2) == i64(10)`), (`false`),
	(`i64(10) == i64(10)`), (`true`),
	(`u64(10) == u64(10)`), (`true`),
	(`u64(2) == u64(10)`), (`false`),
	(`"2" == "2"`), (`true`),
	(`"2" == "3"`), (`false`),
	(`"2" != "2"`), (`false`),
	(`"2" != "3"`), (`true`),
	(`i64(2) != i64(10)`), (`true`),
	(`i64(2) != i64(2)`), (`false`),
	(`u64(2) != u64(10)`), (`true`),
	(`u64(2) != u64(2)`), (`false`),
	(`true != false`), (`true`),
	(`true != true`), (`false`),
	(`true < false`), (`false`),
	(`false < true`), (`true`),
	(`true <= false`), (`false`),
	(`false <= true`), (`true`),
	(`"2" * "4"`), (`8`),
	(`"2" + "4"`), (`24`),
	(`i64(2) * i64(4)`), (`8`),
	(`u64(2) * u64(4)`), (`8`),
	(`i64(8) / i64(2)`), (`4`),
	(`u64(8) / u64(2)`), (`4`),
	(`2 <= 4`), (`true`),
	(`4 <= 2`), (`false`),
	(`i64(2) <= i64(4)`), (`true`),
	(`i64(4) <= i64(2)`), (`false`),
	(`u64(2) <= u64(4)`), (`true`),
	(`u64(4) <= u64(2)`), (`false`),
	(`"2" < "2"`), (`false`),
	(`"2" < "3"`), (`true`),
	(`"10" < "2"`), (`true`),
	(`i64(2) < i64(2)`), (`false`),
	(`i64(2) < i64(3)`), (`true`),
	(`u64(2) < u64(2)`), (`false`),
	(`u64(2) < u64(3)`), (`true`),
	(`"2" <= "1"`), (`false`),
	(`"2" <= "2"`), (`true`),
	(`"2" <= "3"`), (`true`),
	(`"10" <= "2"`), (`true`),
	(`true && false`), (`false`),
	(`true || false`), (`true`),
	(`"1" || false`), (`true`),
	(`1 || false`), (`true`),
	(`0 || false`), (`false`),
	(`100 + blank_err`), (`ReferenceError: blank_err is not defined`),
	(`100 + custom_err`), (`ReferenceError: hiya`),
	(`"a \u\"567"`), (`SyntaxError`),
	(`(hello) + (jello`), (`ReferenceError: hello is not defined`),
	(`(1) + (jello`), (`SyntaxError`),
	(`(1) && `), (`SyntaxError`),
	(` && (1)`), (`SyntaxError`),
	(`1 < (}2) < (1)`), (`SyntaxError`),
	(`1 + - 2`), (`-1`),
	(`1 +`), (`SyntaxError`),
	(`-1 + 2`), (`1`),
	(`/1`), (`SyntaxError`),
	(`10 % 2`), (`0`),
	(`10 % 3`), (`1`),
	(`i64(10) % i64(3)`), (`1`),
	(`u64(10) % u64(3)`), (`1`),
	(`"10" % "3"`), (`1`),
	(`(1 || (2 > 5)) && (4 < 5 || 5 < 4)`), (`true`),
	(`true == !!true`), (`true`),
	(`true == !!true == !false`), (`true`),
	(`true == ! ! true == !false`), (`true`),
	(`true == ! ! true == ! ( 1 == 2 ) `), (`true`),
	(`cust(123)`), (`123`),
	(`cust(1) + cust(4)`), (`5`),
	(`cust(1) - cust(4)`), (`-3`),
	(`cust(2) * cust(4)`), (`8`),
	(`cust(2) / cust(4)`), (`0.5`),
	(`cust(10) % cust(3)`), (`1`),
	(`cust(10) < cust(3)`), (`false`),
	(`cust(10) <= cust(3)`), (`false`),
	(`cust(10) > cust(3)`), (`true`),
	(`cust(10) >= cust(3)`), (`true`),
	(`cust(10) == cust(3)`), (`false`),
	(`cust(10) != cust(3)`), (`true`),
	(`cust(10) && cust(0)`), (`false`),
	(`cust(10) || cust(3)`), (`true`),
	(`cust(10) || cust(3)`), (`true`),
	(`-cust(999)`), (`OperatorError: not this time`), // special error
	(`cust(-90909090) + cust(-90909090)`), (`OperatorError: undefined`),
	(`cust(-80808080) + cust(-80808080)`), (`OperatorError: bad news`),
	(`0x1`), (`1`),
	(`0xZ`), (`SyntaxError`),
	(`Infinity`), (`Infinity`),
	(`-Infinity`), (`-Infinity`),
	(`0xFFFFFFFF`), (`4294967295`),
	(`0xFFFFFFFF+1`), (`4294967296`),
	(`0xFFFFFFFFFFFFFFFF`), (`18446744073709552000`),
	(`0xFFFFFFFFFFFFFFFF+1`), (`18446744073709552000`),
	(`true ? 1 : 2`), (`1`),
	(`false ? 1 : 2`), (`2`),
	(`false ? 1 : true ? 2 : 3`), (`2`),
	(`false ? 1 : false ? 2 : 3`), (`3`),
	(`5*2-10 ? 1 : (3*3-9 < 1 || 6+6-12 ? 8 : false) ? 2 : 3`), (`2`),
	(`(false ? 1 : 2`), (`SyntaxError`),
	(`(false) ? (0xTT) : (0xTT)`), (`SyntaxError`),
	(`(true) ? (0xTT) : (0xTT)`), (`SyntaxError`),
	(`(true) ? (0xTT) : (0xTT`), (`SyntaxError`),
	(`(true) ? (0xTT) 123`), (`SyntaxError`),
	(`(0xTT) ? (0xTT) : 123`), (`SyntaxError`),
	(`1e+10 > 0 ? "big" : "small"`), (`big`),
	(`undefined`), (`undefined`),
	(`true ? () : ()`), (`SyntaxError`),
	(`undefined + 10`), (`NaN`),
	(`null`), (`null`),
	(`null + 10`), (`10`),
	(`undefined + undefined`), (`NaN`),
	(`null + null`), (`0`),
	(`null + undefined`), (`NaN`),
	(`!undefined`), (`true`),
	(`!!undefined`), (`false`),
	(`!null`), (`true`),
	(`!!null`), (`false`),
	(`null??1`), (`1`),
	(`null??0`), (`0`),
	(`undefined??1+1`), (`2`),
	(`undefined??0+1`), (`1`),
	(`false??1+1`), (`false`),
	(`true??1+1`), (`true`),
	(`false??1+1`), (`false`),
	(`true??1+1`), (`true`),
	(`(false??1)+1`), (`1`),
	(`(true??1)+1`), (`2`),
	(`(undefined_noerr??undefined_noerr)+1`), (`NaN`),
	(`(cust(1)??cust(2))+1`), (`2`),
	("`hello world`"), ("hello world"),
	("`hello \"\" world`"), (`hello "" world`),
	("'hello \\'\"\"\\a\\xFF\\p world'"), (`hello '""aÿp world`),
	("'\\xFG'"), (`SyntaxError`),
	(`"\u{A}"`), ("\n"),
	(`"\u{21}"`), ("!"),
	(`"\u{AFFF}"`), ("꿿"),
	(`"\u{1f516}"`), ("🔖"),
	(`"\v"`), ("\v"),
	(`"\0"`), (string(byte(0))),
	(`"\u{YY}"`), ("SyntaxError"),
	(`"\u{FF`), ("SyntaxError"),
	(`1,2,3,4`), ("4"),
	(`1=,2,3,4`), ("SyntaxError"),
	(`1(,2,3,4`), ("SyntaxError"),
	(`1,2,3,(4+)`), ("SyntaxError"),
	(`6<7 , 2>5 , 5`), ("5"),
}

func simpleExtendorOptions(
	udata any,
	eval func(expr string, udata any) (Value, error),
	op func(op Op, a, b Value, udata any) (Value, error),
) Options {
	return Options{UserData: udata, Extender: NewExtender(eval, op)}
}

func TestEval(t *testing.T) {
	testOptions := simpleExtendorOptions(nil,
		func(expr string, _ any) (Value, error) {
			if strings.HasPrefix(expr, "i64(") && expr[len(expr)-1] == ')' {
				x, err := strconv.ParseInt(expr[4:len(expr)-1], 10, 64)
				if err != nil {
					return Undefined, err
				}
				return Int64(x), nil
			}
			if strings.HasPrefix(expr, "u64(") && expr[len(expr)-1] == ')' {
				x, err := strconv.ParseUint(expr[4:len(expr)-1], 10, 64)
				if err != nil {
					return Undefined, err
				}
				return Uint64(x), nil
			}
			if expr == "undefined_noerr" {
				return Undefined, nil
			}
			if expr == "blank_err" {
				return Undefined, ErrUndefined
			}
			if expr == "custom_err" {
				return Undefined, errors.New("hiya")
			}
			if strings.HasPrefix(expr, "cust(") && expr[len(expr)-1] == ')' {
				x, err := strconv.ParseInt(expr[5:len(expr)-1], 10, 64)
				if err != nil {
					return Undefined, err
				}
				return Custom(x), nil
			}
			return Undefined, ErrUndefined
		},
		func(op Op, a, b Value, udata interface{}) (Value, error) {
			if a.Number() == -90909090 || b.Number() == -90909090 {
				// special condition
				return Undefined, ErrUndefined
			}
			if a.Number() == -80808080 || b.Number() == -80808080 {
				// special condition
				return Undefined, errors.New("bad news")
			}
			switch op {
			case OpAdd:
				return Number(a.Number() + b.Number()), nil
			case OpSub:
				return Number(a.Number() - b.Number()), nil
			case OpMul:
				if a.Number() == 999 || b.Number() == 999 {
					return Undefined, errors.New("not this time")
				}
				return Number(a.Number() * b.Number()), nil
			case OpDiv:
				return Number(a.Number() / b.Number()), nil
			case OpMod:
				return Number(math.Mod(a.Number(), b.Number())), nil
			case OpLt:
				return Bool(a.Number() < b.Number()), nil
			case OpLte:
				return Bool(a.Number() <= b.Number()), nil
			case OpGt:
				return Bool(a.Number() > b.Number()), nil
			case OpGte:
				return Bool(a.Number() >= b.Number()), nil
			case OpEq:
				return Bool(a.Number() == b.Number()), nil
			case OpNeq:
				return Bool(a.Number() != b.Number()), nil
			case OpAnd:
				return Bool(a.Bool() && b.Bool()), nil
			case OpOr:
				return Bool(a.Bool() || b.Bool()), nil
			case OpCoal:
				if !a.Bool() {
					return b, nil
				}
				return a, nil
			default:
				return Undefined, ErrUndefined
			}
		},
	)
	for i := 0; i < len(testTable)-1; i += 2 {
		expr, expect := testTable[i], testTable[i+1]
		val, err := Eval(expr, &testOptions)
		if err != nil {
			val = String(err.Error())
		}
		if val.String() != expect {
			t.Fatalf("%d: for '%s' expected '%s' got '%s'",
				i/2, expr, expect, val)
		}
	}

	eval := func(expr string, opts *Options) Value {
		r, err := Eval(expr, opts)
		if err != nil {
			return String(err.Error())
		}
		return r
	}

	if eval("true && false", &testOptions).Bool() == true {
		t.Fatal()
	}
	if eval("true && true", &testOptions).Bool() == false {
		t.Fatal()
	}
	if eval("true && true", &testOptions).Float64() != 1 {
		t.Fatal()
	}
	if eval("true && false", &testOptions).Float64() != 0 {
		t.Fatal()
	}
	if eval("true && true", &testOptions).Int64() != 1 {
		t.Fatal()
	}
	if eval("true && false", &testOptions).Int64() != 0 {
		t.Fatal()
	}
	if eval("true && true", &testOptions).Uint64() != 1 {
		t.Fatal()
	}
	if eval("true && false", &testOptions).Uint64() != 0 {
		t.Fatal()
	}
	if eval("i64(123)", &testOptions).Uint64() != 123 {
		t.Fatal()
	}
	if eval("u64(123)", &testOptions).Uint64() != 123 {
		t.Fatal()
	}
	if eval("i64(123)", &testOptions).Int64() != 123 {
		t.Fatal()
	}
	if eval("i64(-123)", &testOptions).Int64() != -123 {
		t.Fatal()
	}
	if eval("u64(123)", &testOptions).Int64() != 123 {
		t.Fatal()
	}
	if eval("u64(1)", nil).String() != "ReferenceError: u64(1) is not defined" {
		t.Fatal()
	}
	if eval("true && false", &testOptions).Value() == true {
		t.Fatal()
	}
	if eval("true && true", &testOptions).Value() == false {
		t.Fatal()
	}
	if eval("123", &testOptions).Value() != float64(123) {
		t.Fatal()
	}
	if eval("\"123\"", &testOptions).Value() != "123" {
		t.Fatal()
	}
	if eval("i64(123)", &testOptions).Value() != int64(123) {
		t.Fatal()
	}
	if eval("u64(123)", &testOptions).Value() != uint64(123) {
		t.Fatal()
	}
	if eval("i64(123)", &testOptions).Value() != int64(123) {
		t.Fatal()
	}
	if eval("i64(-123)", &testOptions).Value() != int64(-123) {
		t.Fatal()
	}
	if eval("u64(123)", &testOptions).Value() != uint64(123) {
		t.Fatal()
	}
	if Custom(1).Value() != 1 {
		t.Fatal()
	}
	if !Custom(1).IsCustom() {
		t.Fatal()
	}
	if Bool(true).IsCustom() {
		t.Fatal()
	}
	if (Value{}).Value() != nil {
		t.Fatal()
	}
	if CharPosOfErr(nil) != -1 {
		t.Fatal()
	}
	_, err := Eval("1 == hello", nil)
	if CharPosOfErr(err) != 5 {
		t.Fatal()
	}
	if Number(90.5) != Float64(90.5) {
		t.Fatal()
	}
	if OpAdd.String() != "+" {
		t.Fatal()
	}
	if OpSub.String() != "-" {
		t.Fatal()
	}
	if OpMul.String() != "*" {
		t.Fatal()
	}
	if OpDiv.String() != "/" {
		t.Fatal()
	}
	if OpMod.String() != "%" {
		t.Fatal()
	}
	if OpLt.String() != "<" {
		t.Fatal()
	}
	if OpLte.String() != "<=" {
		t.Fatal()
	}
	if OpGt.String() != ">" {
		t.Fatal()
	}
	if OpGte.String() != ">=" {
		t.Fatal()
	}
	if OpEq.String() != "==" {
		t.Fatal()
	}
	if OpNeq.String() != "!=" {
		t.Fatal()
	}
	if OpAnd.String() != "&&" {
		t.Fatal()
	}
	if OpOr.String() != "||" {
		t.Fatal()
	}
	if OpCoal.String() != "??" {
		t.Fatal()
	}
	if Op(-1).String() != "" {
		t.Fatal()
	}
	sops := simpleExtendorOptions(nil,
		func(expr string, _ interface{}) (Value, error) {
			return Custom("hello"), nil
		},
		func(op Op, a, b Value, udata any) (Value, error) {
			return Undefined, ErrUndefined
		},
	)
	_, err = Eval("u64(1) + 1", &sops)
	if err == nil || err.Error() != "OperatorError: undefined" {
		t.Fatal()
	}

	sops = simpleExtendorOptions(nil,
		func(expr string, _ interface{}) (Value, error) {
			return Custom(thing(999)), nil
		},
		func(op Op, a, b Value, udata any) (Value, error) {
			return String(fmt.Sprintf("[%d:%d:%t:%s:%.0f][%d:%d:%t:%s:%.0f]",
				a.Int64(), a.Uint64(), a.Bool(), a.String(), a.Float64(),
				a.Int64(), a.Uint64(), a.Bool(), a.String(), a.Float64(),
			)), nil
		},
	)
	v, _ := Eval("abc + 1", &sops)
	if v.String() != "[999:999:true:999:999][999:999:true:999:999]" {
		t.Fatal()
	}
	sops = simpleExtendorOptions(nil, nil, nil)
	if eval("abc + 1", &sops).String() != "ReferenceError: abc is not defined" {
		t.Fatal()
	}
	sops = simpleExtendorOptions(nil,
		func(expr string, _ interface{}) (Value, error) {
			return Custom(thing(999)), nil
		}, nil)
	if eval("abc + 1", &sops).String() != "OperatorError: undefined" {
		t.Fatal()
	}
}

type thing float64

func (t thing) Int64() int64     { return int64(t) }
func (t thing) Uint64() uint64   { return uint64(t) }
func (t thing) Float64() float64 { return float64(t) }
func (t thing) Bool() bool       { return float64(t) != 0 }
func (t thing) String() string {
	return strconv.FormatFloat(float64(t), 'f', -1, 64)
}

func FuzzExpr(f *testing.F) {
	// test for panics
	for i := 0; i < len(testTable)-1; i += 2 {
		f.Add(testTable[i])
	}
	f.Add(`(askdflkasdj`)
	f.Add(`(askdflkasdj || ^ !)(*!@#$^%$!# || <= "7" >= " 1`)
	f.Add(`(9 <= 10.123 && 9 >= -5.0) || "hello" != "jello"`)
	f.Fuzz(func(t *testing.T, expr string) {
		Eval(expr, nil)
	})
}

func testParseString(t *testing.T, data, expect string, expectOK bool) {
	t.Helper()
	got, ok := parseString(data)
	if ok != expectOK || got != expect {
		t.Fatalf("expected %t/'%s' got %t/'%s'", expectOK, expect, ok, got)
	}
}
func TestParseString(t *testing.T) {
	testParseString(t, ``, ``, false)
	testParseString(t, `"`, ``, false)
	testParseString(t, `"\`, ``, false)
	testParseString(t, `"1"1`, ``, false)
	testParseString(t, "``", ``, true)
	testParseString(t, "`1`", `1`, true)
	testParseString(t, "`2`", `2`, true)
	testParseString(t, "`2\"`", `2"`, true)
	testParseString(t, "'2'", `2`, true)
	testParseString(t, "'2\\1'", `21`, true)
	testParseString(t, `'hh"ii'`, `hh"ii`, true)
}

func BenchmarkSimpleFact(b *testing.B) {
	for i := 0; i < b.N; i++ {
		Eval("5 * 10", nil)
	}
}

func BenchmarkSimpleFactRef(b *testing.B) {
	opts := simpleExtendorOptions(nil,
		func(expr string, _ interface{}) (Value, error) {
			if expr == "ten" {
				return Float64(10), nil
			}
			return Undefined, nil
		}, nil,
	)
	for i := 0; i < b.N; i++ {
		Eval("5 * ten", &opts)
	}
}

func BenchmarkSimpleComp(b *testing.B) {
	for i := 0; i < b.N; i++ {
		Eval("5 < 10", nil)
	}
}

func BenchmarkSimpleCompRef(b *testing.B) {
	opts := simpleExtendorOptions(nil,
		func(expr string, _ interface{}) (Value, error) {
			if expr == "ten" {
				return Float64(10), nil
			}
			return Undefined, nil
		}, nil,
	)
	for i := 0; i < b.N; i++ {
		Eval("5 < ten", &opts)
	}
}

func TestReadme(t *testing.T) {
	// Create a user data map that can be referenced by the Eval function.
	umap := make(map[string]Value)

	// Add a bounding box to the user data map.
	umap["minX"] = Number(112.8192)
	umap["minY"] = Number(33.4738)
	umap["maxX"] = Number(113.9146)
	umap["maxY"] = Number(34.3367)

	// Add a timestamp value to the user data map.
	ts, _ := time.Parse(time.RFC3339, "2022-03-31T09:00:00Z")
	umap["timestamp"] = Custom(ts)

	// Set up an evaluation extender for referencing the user data and
	// using operators on custom types.
	ext := NewExtender(
		func(expr string, udata any) (Value, error) {
			switch expr {
			case "now":
				// Get the seconds since Epoch.
				return Custom(time.Now()), nil
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
				umap, ok := udata.(map[string]Value)
				if !ok {
					return Undefined, ErrUndefined
				}
				return umap[expr], nil
			}
		},
		func(op Op, a, b Value, udata any) (Value, error) {
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
					return Custom(x.Add(time.Duration(y))), nil
				case OpSub:
					return Custom(x.Add(-time.Duration(y))), nil
				}
			}
			return Undefined, ErrUndefined
		},
	)

	// Set up the options
	opts := Options{UserData: umap, Extender: ext}

	var res Value

	// Return the timestamp.
	res, _ = Eval(`timestamp`, &opts)
	fmt.Println(res)

	// Subtract an hour from the timestamp.
	res, _ = Eval(`timestamp - $1h`, &opts)
	fmt.Println(res)

	// Add one day to the current time.
	res, _ = Eval(`now + $24h`, &opts)
	fmt.Println(res)

	// See if timestamp is older than a day
	res, _ = Eval(`timestamp < now - $24h ? "old" : "new"`, &opts)
	fmt.Println(res)

	// Get the center of the bounding box as a concatenated string.
	res, _ = Eval(`((minX + maxX) / 2) + "," + ((minY + maxY) / 2)`, &opts)
	fmt.Println(res)

	// Output:
	// 2022-03-31 09:00:00 +0000 UTC
	// 2022-03-31 08:00:00 +0000 UTC
	// 2022-04-02 06:00:40.834656 -0700 MST m=+86400.000714835
	// old
	// 113.36689999999999,33.905249999999995
}
