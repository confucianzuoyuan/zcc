package main

import (
	"fmt"
	"os"
	"os/exec"
	"strings"
	"syscall"
)

var opt_cc1 bool
var opt_hash_hash_hash bool
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
		if arg == "-###" {
			opt_hash_hash_hash = true
			continue
		}

		if arg == "-cc1" {
			opt_cc1 = true
			continue
		}

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

func runSubprocess(args []string) {
	// If -### is given, dump the subprocess's command line.
	if opt_hash_hash_hash {
		fmt.Fprintln(os.Stderr, strings.Join(args, " "))
	}

	// 创建命令
	cmd := exec.Command(args[0], args[1:]...)

	// 将子进程的标准输入/输出/错误继承自当前进程
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	// 运行命令并等待完成
	err := cmd.Run()

	if err != nil {
		// 如果是退出错误，可以获取退出状态码
		if exitError, ok := err.(*exec.ExitError); ok {
			// 获取子进程退出状态
			if status, ok := exitError.Sys().(syscall.WaitStatus); ok {
				if status.ExitStatus() != 0 {
					os.Exit(1)
				}
			} else {
				// 无法获取退出状态，直接退出
				os.Exit(1)
			}
		} else {
			// 其他错误，比如命令找不到
			fmt.Fprintf(os.Stderr, "exec failed: %s: %v\n", args[0], err)
			os.Exit(1)
		}
	}
}

func run_cc1(args []string) {
	args = append(args, "-cc1")
	runSubprocess(args)
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

func cc1() {
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

func main() {
	args := os.Args
	parseArgs(args[1:])

	if opt_cc1 {
		cc1()
		return
	}

	run_cc1(args)
}
