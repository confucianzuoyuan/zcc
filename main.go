package main

import (
	"fmt"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"syscall"
)

var opt_S bool
var opt_cc1 bool
var opt_hash_hash_hash bool
var opt_o string = ""
var dump_ir bool = false
var baseFile string = ""
var outputFile string = ""

var inputPaths []string
var tmpfiles []string

func usage(status int) {
	fmt.Fprintf(os.Stderr, "zcc [ -o <path> ] <file>\n")
	fmt.Fprintf(os.Stderr, "      --dump-ir <file>\n")
	os.Exit(status)
}

func takeArg(arg string) bool {
	return arg == "-o"
}

func parseArgs(args []string) {
	// Make sure that all command line options that take an argument
	// have an argument.
	for idx := 1; idx < len(args); idx += 1 {
		if takeArg(args[idx]) {
			if idx+1 == len(args) {
				usage(1)
			}
		}
	}

	for idx := 1; idx < len(args); idx += 1 {
		if args[idx] == "-###" {
			opt_hash_hash_hash = true
			continue
		}

		if args[idx] == "-cc1" {
			opt_cc1 = true
			continue
		}

		if args[idx] == "--help" {
			usage(0)
		}

		if args[idx] == "--dump-ir" {
			if len(args) == 1 {
				usage(1)
			}
			continue
		}

		if args[idx] == "-o" {
			idx += 1
			opt_o = args[idx]
			continue
		}

		if strings.HasPrefix(args[idx], "-o") {
			opt_o = args[idx][2:]
			continue
		}

		if args[idx] == "-S" {
			opt_S = true
			continue
		}

		if args[idx] == "-cc1-input" {
			baseFile = args[idx+1]
			idx += 1
			continue
		}

		if args[idx] == "-cc1-output" {
			outputFile = args[idx+1]
			idx += 1
			continue
		}

		if strings.HasPrefix(args[idx], "-") && len(args[idx]) > 1 {
			e := fmt.Sprintf("unknown argument: %s", args[idx])
			panic(e)
		}

		inputPaths = append(inputPaths, args[idx])
	}

	if len(inputPaths) == 0 {
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

	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	if err := cmd.Run(); err != nil {
		if exitError, ok := err.(*exec.ExitError); ok {
			if status, ok := exitError.Sys().(syscall.WaitStatus); ok {
				os.Exit(status.ExitStatus())
			}
		}
		fmt.Fprintf(os.Stderr, "exec failed: %s: %v\n", args[0], err)
		os.Exit(1)
	}
}

func run_cc1(args []string, input string, output string) {
	args = append(args, "-cc1")

	if input != "" {
		args = append(args, "-cc1-input")
		args = append(args, input)
	}

	if output != "" {
		args = append(args, "-cc1-output")
		args = append(args, output)
	}

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

// Replace file extension
func replaceExtension(tmpl string, extn string) string {
	// 取得文件名（去除路径）
	filename := filepath.Base(tmpl)

	// 找最后一个点的位置
	dot := strings.LastIndex(filename, ".")
	if dot != -1 {
		filename = filename[:dot] // 去除扩展名
	}

	// 拼接新的扩展名
	return filename + extn
}

func cleanup() {
	for _, path := range tmpfiles {
		err := os.Remove(path)
		if err != nil {
			// 打印错误但继续删除其他文件
			log.Printf("failed to remove %s: %v", path, err)
		}
	}
	// 清空切片（可选）
	tmpfiles = nil
}

func createTmpfile() string {
	// ioutil.TempFile 会自动替换模板中的随机部分
	// 但模板格式是目录和前缀，不支持中间的 XXXXXX 格式
	// 所以用 "/tmp" 目录和 "chibicc-" 作为前缀
	tmpFile, err := os.CreateTemp("/tmp", "zcc-")
	if err != nil {
		log.Fatalf("mkstemp failed: %v", err)
	}

	path := tmpFile.Name()

	// 关闭文件描述符
	tmpFile.Close()

	// 记录临时文件路径
	tmpfiles = append(tmpfiles, path)

	return path
}

func cc1() {
	// Tokenize and parse.
	tok := tokenizeFile(baseFile)
	o := parse(tok)

	// Traverse the AST to emit assembly.
	if dump_ir {
		println("dump ir not impl")
	} else {
		out, _ := openFile(outputFile)
		fmt.Fprintf(out, ".file 1 \"%s\"\n", baseFile)
		codegen(o, out)
	}
}

func assemble(input string, output string) {
	cmd := []string{"as", "-c", input, "-o", output}
	runSubprocess(cmd)
}

func main() {
	defer cleanup()
	args := os.Args
	parseArgs(args)

	if opt_cc1 {
		cc1()
		return
	}

	if len(inputPaths) > 1 && opt_o != "" {
		panic("cannot specify '-o' with multiple files")
	}

	for _, p := range inputPaths {
		input := p
		output := ""

		if opt_o != "" {
			output = opt_o
		} else if opt_S {
			output = replaceExtension(input, ".s")
		} else {
			output = replaceExtension(input, ".o")
		}

		// If -S is given, assembly text is the final output.
		if opt_S {
			run_cc1(args, input, output)
			continue
		}

		// Otherwise, run the assembler to assemble our output.
		tmpfile := createTmpfile()
		run_cc1(args, input, tmpfile)
		assemble(tmpfile, output)
	}
}
