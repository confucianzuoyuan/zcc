package main

import (
	"bufio"
	"bytes"
	"fmt"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"syscall"
)

type FileType int

const (
	FILE_NONE FileType = iota
	FILE_C
	FILE_ASM
	FILE_OBJ
	FILE_AR
	FILE_DSO
)

var includePaths []string
var ldExtraArgs []string
var stdIncludePaths []string

var opt_fpic bool
var opt_x FileType
var opt_include []string
var opt_fcommon bool = true
var opt_E bool
var opt_M bool
var opt_MD bool
var opt_MMD bool
var opt_MP bool
var opt_S bool
var opt_c bool
var opt_cc1 bool
var opt_hash_hash_hash bool
var opt_MF string
var opt_MT string
var opt_o string = ""
var dump_ir bool = false
var baseFile string = ""
var outputFile string = ""

var inputPaths []string
var tmpfiles []string

func quoteMakefile(s string) string {
	buf := make([]uint8, len(s)+1)

	i := 0
	j := 0
	for ; i < len(s); i++ {
		switch s[i] {
		case '$':
			buf[j] = '$'
			j++
			buf[j] = '$'
			j++
		case '#':
			buf[j] = '\\'
			j++
			buf[j] = '#'
			j++
		case ' ', '\t':
			for k := i - 1; k >= 0 && s[k] == '\\'; k-- {
				buf[j] = '\\'
				j++
			}
			buf[j] = '\\'
			j++
			buf[j] = s[i]
			j++
		default:
			buf[j] = s[i]
			j++
		}
	}

	return string(buf[:len(buf)-1])
}

func define(str string) {
	eq := strings.Index(str, "=")
	if eq >= 0 {
		name := str[:eq]
		value := str[eq+1:]
		defineMacro(name, value)
	} else {
		defineMacro(str, "1")
	}
}

func usage(status int) {
	fmt.Fprintf(os.Stderr, "zcc [ -o <path> ] <file>\n")
	fmt.Fprintf(os.Stderr, "      --dump-ir <file>\n")
	os.Exit(status)
}

func takeArg(arg string) bool {
	return arg == "-o" || arg == "-I" || arg == "-idirafter" || arg == "-include" || arg == "-x" || arg == "-MF" || arg == "-MT"
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

	idirafter := []string{}

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

		if strings.HasPrefix(args[idx], "-I") {
			includePaths = append(includePaths, args[idx][2:])
			continue
		}

		if args[idx] == "-S" {
			opt_S = true
			continue
		}

		if args[idx] == "-fcommon" {
			opt_fcommon = true
			continue
		}

		if args[idx] == "-fno-common" {
			opt_fcommon = false
			continue
		}

		if args[idx] == "-c" {
			opt_c = true
			continue
		}

		if args[idx] == "-E" {
			opt_E = true
			continue
		}

		if args[idx] == "-D" {
			idx += 1
			define(args[idx])
			continue
		}

		if strings.HasPrefix(args[idx], "-D") {
			define(args[idx][2:])
			continue
		}

		if args[idx] == "-U" {
			idx += 1
			undefMacro(args[idx])
			continue
		}

		if strings.HasPrefix(args[idx], "-U") {
			undefMacro(args[idx][2:])
			continue
		}

		if args[idx] == "-include" {
			idx++
			opt_include = append(opt_include, args[idx])
			continue
		}

		if args[idx] == "-x" {
			idx++
			opt_x = parseOptX(args[idx])
			continue
		}

		if strings.HasPrefix(args[idx], "-x") {
			opt_x = parseOptX(args[idx][2:])
			continue
		}

		if strings.HasPrefix(args[idx], "-l") {
			inputPaths = append(inputPaths, args[idx])
			continue
		}

		if args[idx] == "-s" {
			ldExtraArgs = append(ldExtraArgs, "-s")
			continue
		}

		if args[idx] == "-M" {
			opt_M = true
			continue
		}

		if args[idx] == "-MF" {
			idx++
			opt_MF = args[idx]
			continue
		}

		if args[idx] == "-MP" {
			opt_MP = true
			continue
		}

		if args[idx] == "-MT" {
			if opt_MT == "" {
				idx++
				opt_MT = args[idx]
			} else {
				idx++
				opt_MT += " " + args[idx]
			}
			continue
		}

		if args[idx] == "-MD" {
			opt_MD = true
			continue
		}

		if args[idx] == "-MQ" {
			if opt_MT == "" {
				idx++
				opt_MT = quoteMakefile(args[idx])
			} else {
				idx++
				opt_MT += " " + quoteMakefile(args[idx])
			}

			continue
		}

		if args[idx] == "-MMD" {
			opt_MMD = true
			opt_MD = true
			continue
		}

		if args[idx] == "-fpic" || args[idx] == "-fPIC" {
			opt_fpic = true
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

		if args[idx] == "-idirafter" {
			idirafter = append(idirafter, args[idx])
			idx++
			continue
		}

		if strings.HasPrefix(args[idx], "-O") || strings.HasPrefix(args[idx], "-W") || strings.HasPrefix(args[idx], "-g") || strings.HasPrefix(args[idx], "-std=") || args[idx] == "-ffreestanding" || args[idx] == "-fno-builtin" || args[idx] == "-fno-omit-frame-pointer" || args[idx] == "-fno-stack-protector" || args[idx] == "-fno-strict-aliasing" || args[idx] == "-m64" || args[idx] == "-mno-red-zone" || args[idx] == "-w" {
			continue
		}

		if strings.HasPrefix(args[idx], "-") && len(args[idx]) > 1 {
			e := fmt.Sprintf("unknown argument: %s", args[idx])
			panic(e)
		}

		inputPaths = append(inputPaths, args[idx])
	}

	includePaths = append(includePaths, idirafter...)

	if len(inputPaths) == 0 {
		panic("no input files")
	}

	// -E implies that the input is the C macro language.
	if opt_E {
		opt_x = FILE_C
	}
}

func inStdIncludePath(path string) bool {
	for i := range stdIncludePaths {
		dir := stdIncludePaths[i]
		length := len(dir)
		if strings.HasPrefix(path, dir) && path[length] == '/' {
			return true
		}
	}

	return false
}

// If -M options is given, the compiler write a list of input files to
// stdout in a format that "make" command can read. This feature is
// used to automate file dependency management.
func printDependencies() {
	path := ""
	if opt_MF != "" {
		path = opt_MF
	} else if opt_MD {
		if opt_o != "" {
			path = opt_o
		} else {
			path = replaceExtension(baseFile, ".d")
		}
	} else if opt_o != "" {
		path = opt_o
	} else {
		path = "-"
	}

	out, _ := openFile(path)
	if opt_MT != "" {
		fmt.Fprintf(out, "%s:", opt_MT)
	} else {
		fmt.Fprintf(out, "%s:", quoteMakefile(replaceExtension(baseFile, ".o")))
	}

	files := getInputFiles()

	for i := 0; i < len(files); i++ {
		if opt_MMD && inStdIncludePath(files[i].Name) {
			continue
		}
		fmt.Fprintf(out, " \\\n  %s", files[i].Name)
	}

	fmt.Fprintf(out, "\n\n")

	if opt_MP {
		for i := 1; i < len(files); i++ {
			if opt_MMD && inStdIncludePath(files[i].Name) {
				continue
			}
			fmt.Fprintf(out, "%s:\n\n", quoteMakefile(files[i].Name))
		}
	}
}

func getFileType(filename string) FileType {
	if opt_x != FILE_NONE {
		return opt_x
	}

	if strings.HasSuffix(filename, ".a") {
		return FILE_AR
	}
	if strings.HasSuffix(filename, ".so") {
		return FILE_DSO
	}
	if strings.HasSuffix(filename, ".o") {
		return FILE_OBJ
	}

	if strings.HasSuffix(filename, ".c") {
		return FILE_C
	}

	if strings.HasSuffix(filename, ".s") {
		return FILE_ASM
	}

	panic("<command line>: unknown file extension: " + filename)
}

func parseOptX(s string) FileType {
	if s == "c" {
		return FILE_C
	}
	if s == "assembler" {
		return FILE_ASM
	}
	if s == "none" {
		return FILE_NONE
	}
	panic("<command line>: unknown argument for -x: " + s)
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

func mustTokenzieFile(path string) *Token {
	tok := tokenizeFile(path)
	if tok == nil {
		panic(path)
	}
	return tok
}

func appendTokens(tok1 *Token, tok2 *Token) *Token {
	if tok1 == nil || tok1.Kind == TK_EOF {
		return tok2
	}

	t := tok1
	for t.Next.Kind != TK_EOF {
		t = t.Next
	}
	t.Next = tok2
	return tok1
}

func cc1() {
	var tok *Token = nil

	// Process -include option
	for i := 0; i < len(opt_include); i++ {
		incl := opt_include[i]

		path := ""
		if fileExists(incl) {
			path = incl
		} else {
			path = searchIncludePaths(incl)
			if path == "" {
				panic(fmt.Sprintf("-include: %s", incl))
			}
		}

		tok2 := mustTokenzieFile(path)
		tok = appendTokens(tok, tok2)
	}

	// Tokenize and parse.
	tok2 := mustTokenzieFile(baseFile)
	tok = appendTokens(tok, tok2)
	tok = preprocess(tok)

	// If -M or -MD are given, print file dependencies.
	if opt_M || opt_MD {
		printDependencies()
		if opt_M {
			return
		}
	}

	// If -E is given, print out preprocessed C code as a result.
	if opt_E {
		printTokens(tok)
		return
	}

	prog := parse(tok)

	// Traverse the AST to emit assembly.
	if dump_ir {
		println("dump ir not impl")
	} else {
		// Open a temporary output buffer.
		var b bytes.Buffer
		buf := bufio.NewWriter(&b)
		codegen(prog, buf)
		buf.Flush()
		out, _ := openFile(outputFile)
		out.Write(b.Bytes())
	}
}

// Print tokens to stdout. Used for -E.
func printTokens(tok *Token) {
	var out *os.File
	if opt_o != "" {
		out, _ = openFile(opt_o)
	} else {
		out, _ = openFile("-")
	}

	line := 1
	for ; tok != nil && tok.Kind != TK_EOF; tok = tok.Next {
		if line > 1 && tok.AtBeginningOfLine {
			fmt.Fprintf(out, "\n")
		}
		if tok.HasSpace && !tok.AtBeginningOfLine {
			fmt.Fprintf(out, " ")
		}
		fmt.Fprintf(out, "%s", B2S((*tok.File.Contents)[tok.Location:tok.Location+tok.Length]))
		line += 1
	}
	fmt.Fprintf(out, "\n")
}

func assemble(input string, output string) {
	cmd := []string{"as", "-c", input, "-o", output}
	runSubprocess(cmd)
}

func findFile(pattern string) (string, error) {
	matches, err := filepath.Glob(pattern)
	if err != nil {
		return "", err
	}
	if len(matches) == 0 {
		return "", nil
	}
	return matches[len(matches)-1], nil
}

func fileExists(path string) bool {
	_, err := os.Stat(path)
	return err == nil
}

func findLibPath() string {
	if fileExists("/usr/lib/x86_64-linux-gnu/crti.o") {
		return "/usr/lib/x86_64-linux-gnu"
	}
	if fileExists("/usr/lib64/crti.o") {
		return "/usr/lib64"
	}
	panic("library path is not found")
}

func findGccLibPath() string {
	paths := []string{
		"/usr/lib/gcc/x86_64-linux-gnu/*/crtbegin.o",
		"/usr/lib/gcc/x86_64-pc-linux-gnu/*/crtbegin.o", // For Gentoo
		"/usr/lib/gcc/x86_64-redhat-linux/*/crtbegin.o", // For Fedora
	}

	for _, p := range paths {
		path, _ := findFile(p)
		if path != "" {
			return filepath.Dir(path)
		}
	}

	panic("gcc library path is not found")
}

func addDefaultIncludePaths(argv0 string) {
	// We expect that chibicc-specific include files are installed
	// to ./include relative to argv[0].
	includePaths = append(includePaths, filepath.Dir(argv0)+"/include")

	// Add standard include paths.
	includePaths = append(includePaths, "/usr/local/include")
	includePaths = append(includePaths, "/usr/include/x86_64-linux-gnu")
	includePaths = append(includePaths, "/usr/include")

	// Keep a copy of the standard include paths for -MMD option.
	stdIncludePaths = append(stdIncludePaths, includePaths...)
}

func runLinker(inputs []string, output string) {
	arr := []string{}

	arr = append(arr, "ld")
	arr = append(arr, "-o")
	arr = append(arr, output)
	arr = append(arr, "-m")
	arr = append(arr, "elf_x86_64")
	arr = append(arr, "-dynamic-linker")
	arr = append(arr, "/lib64/ld-linux-x86-64.so.2")

	libpath := findLibPath()
	gccLibpath := findGccLibPath()

	arr = append(arr, libpath+"/crt1.o")
	arr = append(arr, libpath+"/crti.o")
	arr = append(arr, gccLibpath+"/crtbegin.o")
	arr = append(arr, "-L"+gccLibpath)
	arr = append(arr, "-L"+libpath)
	arr = append(arr, "-L"+libpath+"/..")
	arr = append(arr, "-L/usr/lib64")
	arr = append(arr, "-L/lib64")
	arr = append(arr, "-L/usr/lib/x86_64-linux-gnu")
	arr = append(arr, "-L/usr/lib/x86_64-pc-linux-gnu")
	arr = append(arr, "-L/usr/lib/x86_64-redhat-linux")
	arr = append(arr, "-L/usr/lib")
	arr = append(arr, "-L/lib")

	arr = append(arr, ldExtraArgs...)
	arr = append(arr, inputs...)

	arr = append(arr, "-lc")
	arr = append(arr, "-lgcc")
	arr = append(arr, "--as-needed")
	arr = append(arr, "-lgcc_s")
	arr = append(arr, "--no-as-needed")
	arr = append(arr, gccLibpath+"/crtend.o")
	arr = append(arr, libpath+"/crtn.o")

	runSubprocess(arr)
}

func main() {
	defer cleanup()
	args := os.Args
	initMacros()
	parseArgs(args)

	if opt_cc1 {
		addDefaultIncludePaths(args[0])
		cc1()
		return
	}

	if len(inputPaths) > 1 && opt_o != "" && (opt_c || opt_S || opt_E) {
		panic("cannot specify '-o' with '-c' ,'-S' or '-E' with multiple files")
	}

	ldArgs := []string{}

	for _, p := range inputPaths {
		input := p
		if strings.HasPrefix(input, "-l") {
			ldArgs = append(ldArgs, input)
			continue
		}
		output := ""

		if opt_o != "" {
			output = opt_o
		} else if opt_S {
			output = replaceExtension(input, ".s")
		} else {
			output = replaceExtension(input, ".o")
		}

		filetype := getFileType(input)

		// Handle .o or .a
		if filetype == FILE_OBJ || filetype == FILE_AR || filetype == FILE_DSO {
			ldArgs = append(ldArgs, input)
			continue
		}

		// Handle .s
		if filetype == FILE_ASM {
			if !opt_S {
				assemble(input, output)
			}
			continue
		}

		if filetype != FILE_C {
			panic("must be .c file")
		}

		// Just preprocess
		if opt_E || opt_M {
			run_cc1(args, input, "")
			continue
		}

		// Compile
		if opt_S {
			run_cc1(args, input, output)
			continue
		}

		// Compile and assemble
		if opt_c {
			tmp := createTmpfile()
			run_cc1(args, input, tmp)
			assemble(tmp, output)
			continue
		}

		// Compile, assemble and link
		tmp1 := createTmpfile()
		tmp2 := createTmpfile()
		run_cc1(args, input, tmp1)
		assemble(tmp1, tmp2)
		ldArgs = append(ldArgs, tmp2)
		continue
	}

	if len(ldArgs) > 0 {
		if opt_o != "" {
			runLinker(ldArgs, opt_o)
		} else {
			runLinker(ldArgs, "a.out")
		}
	}
}
