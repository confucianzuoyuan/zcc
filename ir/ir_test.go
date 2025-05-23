package ir

import (
	"errors"
	"fmt"
	"math/rand"
	"regexp"
	"strings"
	"testing"
)

// randomFormat returns one of a set of greeting messages. The returned
// message is selected at random.
func randomFormat() string {
	// A slice of message formats.
	formats := []string{
		"Hi, %v. Welcome!",
		"Great to see you, %v!",
		"Hail, %v! Well met!",
	}

	// Return one of the message formats selected at random.
	return formats[rand.Intn(len(formats))]
}

// Hello returns a greeting for the named person.
func Hello(name string) (string, error) {
	// If no name was given, return an error with a message.
	if name == "" {
		return name, errors.New("empty name")
	}
	// Create a message using a random format.
	message := fmt.Sprintf(randomFormat(), name)
	return message, nil
}

// TestHelloName calls greetings.Hello with a name, checking
// for a valid return value.
func TestHelloName(t *testing.T) {
	name := "Gladys"
	want := regexp.MustCompile(`\b` + name + `\b`)
	msg, err := Hello("Gladys")
	if !want.MatchString(msg) || err != nil {
		t.Errorf(`Hello("Gladys") = %q, %v, want match for %#q, nil`, msg, err, want)
	}
}

func TestIrValue(t *testing.T) {
	// Test IrValueTemporary
	temp := &IrValueTemporary{Name: "temp42"}
	if temp.String() != "%temp42" {
		t.Errorf("Expected %%temp, got %s", temp.String())
	}

	// Test IrValueGlobal
	global := &IrValueGlobal{Name: "main"}
	if global.String() != "$main" {
		t.Errorf("Expected $main, got %s", global.String())
	}

	// Test IrValueConst
	constant := &IrValueConst{Value: 1337}
	if constant.String() != "1337" {
		t.Errorf("Expected 1337, got %s", constant.String())
	}
}

func TestIrBlock(t *testing.T) {
	block := &IrBlock{
		Label: "start",
		Items: []IrBlockItem{&IrStatementBlockItem{&VolatileStmt{Instr: &RetInstr{Value: nil}}}},
	}

	formatted := block.String()
	lines := strings.Split(formatted, "\n")
	if lines[0] != "@start" {
		t.Errorf("Expected 'start:', got '%s'", lines[0])
	}
	if lines[1] != "\tret" {
		t.Errorf("Expected '\tret', got '%s'", lines[1])
	}

	block = &IrBlock{
		"start",
		[]IrBlockItem{
			&IrCommentBlockItem{C: "Comment"},
			&IrStatementBlockItem{
				&AssignStmt{
					&IrValueTemporary{Name: "foo"},
					&IrTypeWord{},
					&AddInstr{
						Lhs: &IrValueConst{2},
						Rhs: &IrValueConst{2},
					}},
			},
			&IrStatementBlockItem{
				&VolatileStmt{&RetInstr{&IrValueTemporary{"foo"}}},
			},
		},
	}

	formatted = block.String()
	lines = strings.Split(formatted, "\n")
	if lines[0] != "@start" {
		t.Errorf("Expected 'start:', got '%s'", lines[0])
	}
	if lines[1] != "\t# Comment" {
		t.Errorf("Expected '\t# Comment', got '%s'", lines[1])
	}
	if lines[2] != "\t%foo =w add 2, 2" {
		t.Errorf("Expected '\t%%foo =w add 2, 2', got '%s'", lines[2])
	}
	if lines[3] != "\tret %foo" {
		t.Errorf("Expected '\tret %%foo', got '%s'", lines[3])
	}
}

func TestIrInstrBlit(t *testing.T) {
	block := &IrBlock{
		"start",
		[]IrBlockItem{
			&IrStatementBlockItem{
				&VolatileStmt{
					&BlitInstr{
						&IrValueTemporary{"src"},
						&IrValueTemporary{"dst"},
						4,
					},
				},
			},
		},
	}

	formatted := block.String()
	lines := strings.Split(formatted, "\n")
	if lines[0] != "@start" {
		t.Errorf("Expected 'start:', got '%s'", lines[0])
	}
	if lines[1] != "\tblit %src, %dst, 4" {
		t.Errorf("Expected '\tblit %%src, %%dst, 4', got '%s'", lines[1])
	}
}

func TestIrFunction(t *testing.T) {
	f := &IrFunction{
		publicLinkage(),
		"main",
		[]IrFunctionArg{},
		nil,
		[]IrBlock{
			{
				"start",
				[]IrBlockItem{
					&IrStatementBlockItem{
						&VolatileStmt{
							&RetInstr{nil},
						},
					},
				},
			},
		},
	}

	println(f.String())
}

func TestDataDef(t *testing.T) {
	datadef := &IrDataDef{
		publicLinkage(),
		"hello",
		-1,
		[]IrDataItemWithType{
			{&IrTypeByte{}, &IrDataItemStr{"Hello, World!"}},
			{&IrTypeByte{}, &IrDataItemConst{0}},
		},
	}

	println(datadef.String())
}
