// Copyright 2022 Joshua J Baker. All rights reserved.
// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file.

package expr

import (
	"errors"
	"fmt"
	"math"
	"reflect"
	"strconv"
	"testing"
	"time"
)

var testTable = []string{
	(`.1`), (`0.1`),
	(`.1e-1`), (`0.01`),
	(`.1e-1 + 5`), (`5.01`),
	(`0.1`), (`0.1`),
	(`1u64`), (`1`),
	(`1.0u64`), (`SyntaxError`),
	(`-1i64`), (`-1`),
	(`-1.0u64`), (`SyntaxError`),
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
	(`0 + {1}`), (`SyntaxError`),
	(`0 + [1]`), (`SyntaxError`),
	(`hello + 2`), (`ReferenceError: hello is not defined`),
	(`i64("-9223372036854775808")`), (`-9223372036854775808`),
	(`-9223372036854775808i64`), (`-9223372036854775808`),
	(`i64("9223372036854775807")`), (`9223372036854775807`),
	(`9223372036854775807i64`), (`9223372036854775807`),
	(`i64("-9223372036854775808")`), (`-9223372036854775808`),
	(`u64("18446744073709551615") - u64("18446744073709551614")`), (`1`),
	(`18446744073709551615u64 - 18446744073709551614u64`), (`1`),
	(`u64("18446744073709551614") + u64("1")`), (`18446744073709551615`),
	(`i64("-9223372036854775808") + i64("1")`), (`-9223372036854775807`),
	(`i64("9223372036854775807") - i64("1")`), (`9223372036854775806`),
	(`i64("9223372036854775807") - 1`), (`9223372036854776000`),
	(`u64("9223372036854775807") - 1`), (`9223372036854776000`),
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
	(`cust(-90909090) + cust(-90909090)`), (`undefined`),
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
	(`hello ?. world`), ("ReferenceError: hello is not defined"),
	(`this?.that("1","2")`), (`ReferenceError: this is not defined`),
	(`  != 100`), ("SyntaxError"),
	(`  >= 100`), ("SyntaxError"),
	(` (1) != ("\'1`), ("SyntaxError"),
	(`1 != 2 > 1 != 1`), ("true"),
	(`1 != 2 < 1 != 1`), ("false"),
	(`1 != 1 < 2 != 1`), ("true"),
	(`u64+"hello"`), ("[Function u64]hello"),
	(`0.123123i64`), (`SyntaxError`),
	(`new`), (`SyntaxError`),
	(`howdy.myfn1().myfn2("1",2,"3") == 6`), (`true`),
	(`howdy.myfn1.there`), (`undefined`),
	(`howdy.myfn3.there`), (`Uncaught TypeError: Cannot read properties of undefined (reading 'there')`),
	(`howdy.myfn3?.there`), (`undefined`),
	(`howdy.myfn1#e`), (`SyntaxError`),
	(`howdy.myfn1.#e`), (`SyntaxError`),
	(`#howdy.myfn1.#e`), (`SyntaxError`),
	(`howdy["do"]`), (`undefined`),
	(`howdy[9i8203]`), (`SyntaxError`),
	(`howdy["did"]`), (`ReferenceError: fantastic`),
	(`howdy.myfn1(9999)`), (`CallError: fantastic`),
	(`((0i64)%0i64)`), (`NaN`),
	(`((0i64)/0i64)`), (`NaN`),
	(`((0u64)%0u64)`), (`NaN`),
	(`((0u64)/0u64)`), (`NaN`),
	(`64`), (`64`),
	(`u64`), (`[Function u64]`),
	(`i64`), (`[Function i64]`),
	(`1 == "1"`), (`true`),
	(`1 === "1"`), (`false`),
	(`1 !== "1"`), (`true`),
	(`"1" === "1"`), (`true`),
	(`"1" === "2"`), (`false`),
	(`"1" !== "2"`), (`true`),
	(`false !== true`), (`true`),
	(`false !== ! true`), (`false`),
}

func simpleExtendorOptions(
	udata any,
	ref func(info RefInfo, ctx *Context) (Value, error),
	call func(info CallInfo, ctx *Context) (Value, error),
	op func(info OpInfo, ctx *Context) (Value, error),
) Context {
	return Context{UserData: udata, Extender: NewExtender(ref, call, op)}
}

type fnValue struct {
	name string
}

func TestEvalTable(t *testing.T) {
	testOptions := simpleExtendorOptions(nil,
		func(info RefInfo, _ *Context) (Value, error) {
			if !info.Chain {
				switch info.Ident {
				case "i64", "u64", "cust":
					return Function(info.Ident), nil
				case "custom_err":
					return Undefined, errors.New("hiya")
				case "howdy":
					return String("hiya"), nil
				}
			} else {
				switch info.Ident {
				case "myfn1":
					return Function(info.Ident), nil
				case "myfn2":
					return Function(info.Ident), nil
				case "did":
					return Undefined, errors.New("fantastic")
				}
			}
			return Undefined, nil
		},
		func(info CallInfo, ctx *Context) (Value, error) {
			args, err := info.Args.Compute()
			if err != nil {
				return Undefined, err
			}
			switch info.Ident {
			case "i64":
				x, _ := strconv.ParseInt(args.Get(0).String(), 10, 64)
				return Int64(x), nil
			case "u64":
				x, _ := strconv.ParseUint(args.Get(0).String(), 10, 64)
				return Uint64(x), nil
			case "cust":
				x, err := strconv.ParseInt(args.Get(0).String(), 10, 64)
				if err != nil {
					return Undefined, err
				}
				return Object(x), nil
			case "myfn1":
				if args.Get(0).String() == "9999" {
					return Undefined, errors.New("fantastic")
				}
				return info.Value, nil
			case "myfn2":
				var sum float64
				for i := 0; i < args.Len(); i++ {
					sum += args.Get(i).Float64()
				}
				return Float64(sum), nil
			}
			return Undefined, nil
		},
		func(info OpInfo, ctx *Context) (Value, error) {
			a := info.Left
			b := info.Right
			op := info.Op
			if a.Number() == -90909090 || b.Number() == -90909090 {
				// special condition
				return Undefined, nil
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
				return Undefined, nil
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
	eval := func(expr string, ctx *Context) Value {
		r, err := Eval(expr, ctx)
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
	if eval("u64(1)", nil).String() != "ReferenceError: u64 is not defined" {
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
	if Object(1).Value() != 1 {
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
	if OpAnd.String() != "&&" {
		t.Fatal()
	}
	if OpOr.String() != "||" {
		t.Fatal()
	}
	if OpCoal.String() != "??" {
		t.Fatal()
	}
	if Op("").String() != "" {
		t.Fatal()
	}
	sops := simpleExtendorOptions(nil,
		func(info RefInfo, ctx *Context) (Value, error) {
			return Object("hello"), nil
		},
		nil,
		func(info OpInfo, ctx *Context) (Value, error) {
			return Undefined, nil
		},
	)
	_, err = Eval("u64(1) + 1", &sops)
	if err == nil || err.Error() != "Uncaught TypeError: u64 is not a function" {
		t.Fatal()
	}

	sops = simpleExtendorOptions(nil,
		func(info RefInfo, ctx *Context) (Value, error) {
			return Object(thing(999)), nil
		},
		nil,
		func(info OpInfo, ctx *Context) (Value, error) {
			a := info.Left
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
	sops = simpleExtendorOptions(nil, nil, nil, nil)
	if eval("abc + 1", &sops).String() != "ReferenceError: abc is not defined" {
		t.Fatal()
	}
	sops = simpleExtendorOptions(nil,
		func(info RefInfo, ctx *Context) (Value, error) {
			return Object(thing(999)), nil
		}, nil, nil)
	if eval("abc + 1", &sops).String() != "undefined" {
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

func TestComputedArgs(t *testing.T) {
	var cargs ComputedArgs
	if cargs.Get(0) != Undefined {
		t.Fatal()
	}
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
	got, raw, ok := parseString(data)
	if ok && len(raw) != len(data) {
		got = ""
		ok = false
	}
	if ok != expectOK || got != expect {
		t.Fatalf("expected %t/'%s' got %t/'%s'", expectOK, expect, ok, got)
	}
}

func TestIdent(t *testing.T) {
	id, ok := readIdent("")
	if id != "" || ok {
		t.Fatal()
	}
}

func TestEvalAtom(t *testing.T) {
	// check various atom cases
	val, err := evalAtom("true", 0, 0, nil)
	if err != nil || !val.Bool() {
		t.Fatal()
	}

	if _, err = evalAtom("hello", 0, 0, nil); err == nil {
		t.Fatal()
	}
	if _, err = evalAtom("(true", 0, 0, nil); err == nil {
		t.Fatal()
	}
	if _, err = evalAtom("true?#", 0, 0, nil); err == nil {
		t.Fatal()
	}
	if _, err = evalAtom("true?#", 0, 0, nil); err == nil {
		t.Fatal()
	}
	if _, err = evalAtom("true(", 0, 0, nil); err == nil {
		t.Fatal()
	}
	if _, err = evalAtom("true[", 0, 0, nil); err == nil {
		t.Fatal()
	}
	ref := func(info RefInfo, ctx *Context) (Value, error) {
		if info.Ident == "myfn" {
			return Function(info.Ident), nil
		}
		return Undefined, nil
	}
	sopts := simpleExtendorOptions(nil, ref, nil, nil)
	if _, err = evalAtom("myfn()", 0, 0, &sopts); err != nil {
		t.Fatal()
	}
}

func TestEvalForEach(t *testing.T) {
	var vals []int
	res, err := EvalForEach(`1,2,3,4`, func(value Value) error {
		vals = append(vals, int(value.Int64()))
		return nil
	}, nil)
	if err != nil {
		t.Fatal(err)
	}
	if res.Int64() != 4 {
		t.Fatal()
	}
	if !reflect.DeepEqual(vals, []int{1, 2, 3, 4}) {
		t.Fatal()
	}
	vals = nil
	res, err = EvalForEach(`1,2,3,4`, func(value Value) error {
		vals = append(vals, int(value.Int64()))
		if len(vals) == 3 {
			return ErrStop
		}
		return nil
	}, nil)
	if err != nil {
		t.Fatal(err)
	}
	if res.Int64() != 3 {
		t.Fatal()
	}
	if !reflect.DeepEqual(vals, []int{1, 2, 3}) {
		t.Fatal()
	}

	vals = nil
	res, err = EvalForEach(`1,2,3,4`, func(value Value) error {
		vals = append(vals, int(value.Int64()))
		if len(vals) == 4 {
			return ErrStop
		}
		return nil
	}, nil)
	if err != nil {
		t.Fatal(err)
	}
	if res.Int64() != 4 {
		t.Fatal()
	}

	if !reflect.DeepEqual(vals, []int{1, 2, 3, 4}) {
		t.Fatal()
	}

	vals = nil
	res, err = EvalForEach(`1,2,3,4`, func(value Value) error {
		vals = append(vals, int(value.Int64()))
		if len(vals) == 3 {
			return errors.New("fail")
		}
		return nil
	}, nil)
	if err == nil {
		t.Fatal()
	}
	vals = nil
	res, err = EvalForEach(`1,2,3,4`, func(value Value) error {
		vals = append(vals, int(value.Int64()))
		if len(vals) == 4 {
			return errors.New("fail")
		}
		return nil
	}, nil)
	if err == nil {
		t.Fatal()
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
		func(info RefInfo, ctx *Context) (Value, error) {
			if info.Ident == "ten" {
				return Float64(10), nil
			}
			return Undefined, nil
		}, nil, nil,
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
		func(info RefInfo, ctx *Context) (Value, error) {
			if info.Ident == "ten" {
				return Float64(10), nil
			}
			return Undefined, nil
		}, nil, nil,
	)
	for i := 0; i < b.N; i++ {
		Eval("5 < ten", &opts)
	}
}

func TestNoDoOp(t *testing.T) {
	_, err := doOp("", Undefined, Undefined, 0, nil)
	if err == nil {
		t.Fatal()
	}
}

func TestTypeOf(t *testing.T) {
	if Number(0).TypeOf() != "number" {
		t.Fatal()
	}
	if Bool(false).TypeOf() != "boolean" {
		t.Fatal()
	}
	if Int64(0).TypeOf() != "number" {
		t.Fatal()
	}
	if Uint64(0).TypeOf() != "number" {
		t.Fatal()
	}
	if Float64(0).TypeOf() != "number" {
		t.Fatal()
	}
	if String("").TypeOf() != "string" {
		t.Fatal()
	}
	if Function("").TypeOf() != "function" {
		t.Fatal()
	}
	if Undefined.TypeOf() != "undefined" {
		t.Fatal()
	}
	if Null.TypeOf() != "object" {
		t.Fatal()
	}
	if Object("hello").TypeOf() != "object" {
		t.Fatal()
	}
}

func TestReadme(t *testing.T) {
	// Create a user data map that can be referenced by the Eval function.
	dict := make(map[string]Value)

	// Add a bounding box to the user dictionary.
	dict["minX"] = Number(112.8192)
	dict["minY"] = Number(33.4738)
	dict["maxX"] = Number(113.9146)
	dict["maxY"] = Number(34.3367)

	// Add a timestamp value to the user dictionary.
	ts, _ := time.Parse(time.RFC3339, "2022-03-31T09:00:00Z")
	dict["timestamp"] = Object(ts)

	// Set up an evaluation extender for referencing the user data, and
	// using functions and operators on custom types.
	ext := NewExtender(
		func(info RefInfo, ctx *Context) (Value, error) {
			if info.Chain {
				// Only use globals in this example.
				// No chained objects like `user.name`.
				return Undefined, nil
			}
			switch info.Ident {
			case "now":
				// The `now()` function
				return Function("now"), nil
			case "dur":
				// The `dur(str)` function
				return Function("duration"), nil
			default:
				// Check the user dictionary.
				umap, ok := ctx.UserData.(map[string]Value)
				if !ok {
					// value not found in
					return Undefined, nil
				}
				return umap[info.Ident], nil
			}
		},
		func(info CallInfo, ctx *Context) (Value, error) {
			if info.Chain {
				// Only use globals in this example.
				// No chained function like `user.name()`.
				return Undefined, nil
			}
			switch info.Ident {
			case "now":
				// Return the current date/time.
				return Object(time.Now()), nil
			case "dur":
				// Compute the arguments.
				args, err := info.Args.Compute()
				if err != nil {
					return Undefined, err
				}
				// Parse the duration using the first argument.
				d, err := time.ParseDuration(args.Get(0).String())
				if err != nil {
					return Undefined, err
				}
				// Valid time.Duration, return as an Int64 value
				return Int64(int64(d)), nil
			default:
				return Undefined, nil
			}
		},
		func(info OpInfo, ctx *Context) (Value, error) {
			// Try to convert a and/or b to time.Time
			left, leftOK := info.Left.Value().(time.Time)
			right, rightOK := info.Right.Value().(time.Time)
			if leftOK && rightOK {
				// Both values are time.Time.
				// Perform comparison operation.
				switch info.Op {
				case OpLt:
					return Bool(left.Before(right)), nil
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
				case OpAdd:
					return Object(x.Add(time.Duration(y))), nil
				case OpSub:
					return Object(x.Add(-time.Duration(y))), nil
				}
			}
			return Undefined, nil
		},
	)

	// Set up a custom context that holds user data and the extender.
	ctx := Context{UserData: dict, Extender: ext}

	var res Value

	// Return the timestamp.
	res, _ = Eval(`timestamp`, &ctx)
	fmt.Println(res)

	// Subtract an hour from the timestamp.
	res, _ = Eval(`timestamp - dur('1h')`, &ctx)
	fmt.Println(res)

	// Add one day to the current time.
	res, _ = Eval(`now() + dur('24h')`, &ctx)
	fmt.Println(res)

	// See if timestamp is older than a day
	res, _ = Eval(`timestamp < now() - dur('24h') ? "old" : "new"`, &ctx)
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
}
