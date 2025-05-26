package main

import (
	"fmt"
	"os"
	"strings"
)

var opt_o string
var dump_ir bool = false
var inputPath string = ""

func usage(status int) {
	fmt.Fprintf(os.Stderr, "zcc [ -o <path> ] <file>\n")
	fmt.Fprintf(os.Stderr, "      --dump-ir <file>\n")
	os.Exit(status)
}

func parseArgs(args []string) {
	for idx, arg := range args {
		if arg == "--help" {
			usage(0)
		}

		if arg == "--dump-ir" {
			if len(args) == 1 {
				usage(1)
			}
			continue
		}

		if arg == "-o" {
			if len(args) == 1 {
				usage(1)
			}
			opt_o = args[idx+1]
			continue
		}

		if strings.HasPrefix(arg, "-o") {
			opt_o = arg[2:]
			continue
		}

		if strings.HasPrefix(arg, "-") && len(arg) > 1 {
			e := fmt.Sprintf("unknown argument: %s", arg)
			panic(e)
		}

		inputPath = arg
	}

	if inputPath == "" {
		panic("no input files")
	}
}

// openFile 函数接受一个路径字符串，如果路径是 "-"，则返回标准输出。
// 否则，尝试打开指定路径的文件进行写入。
func openFile(path string) (*os.File, error) {
	if path == "" || path == "-" {
		return os.Stdout, nil
	}

	out, err := os.OpenFile(path, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0644)
	if err != nil {
		return nil, fmt.Errorf("cannot open output file: %s: %w", path, err)
	}
	return out, nil
}

func main() {
	args := os.Args[1:]
	parseArgs(args)

	// Tokenize and parse.
	tok := tokenizeFile(inputPath)
	o := parse(tok)

	// Traverse the AST to emit assembly.
	if dump_ir {
		println("============")
	} else {
		out, _ := openFile(opt_o)
		fmt.Fprintf(out, ".file 1 \"%s\"\n", inputPath)
		codegen(o, out)
	}
}
