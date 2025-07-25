package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"syscall"
)

type StdVer int

const (
	STD_NONE StdVer = iota
	STD_C89
	STD_C99
	STD_C11
	STD_C17
	STD_C23
)

type FileType int

const (
	FILE_NONE FileType = iota
	FILE_C
	FILE_ASM
	FILE_OBJ
	FILE_AR
	FILE_DSO
	FILE_PP_ASM
)

var includePaths []string
var ldExtraArgs []string
var stdIncludePaths []string

var opt_cc1_asm_pp bool
var opt_std StdVer
var opt_data_sections bool
var opt_func_sections bool
var opt_g bool
var opt_optimize bool = true
var opt_fpic bool
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
var opt_static bool
var opt_shared bool
var opt_P bool
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

func setCStandard(val int) {
	if val == 89 || val == 90 {
		opt_std = STD_C89
	} else if val == 99 {
		opt_std = STD_C99
	} else if val == 11 {
		opt_std = STD_C11
	} else if val == 17 || val == 18 {
		opt_std = STD_C17
	} else if val == 23 {
		opt_std = STD_C23
	} else {
		panic("unknown c standard")
	}
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
	return arg == "-o" || arg == "-I" || arg == "-idirafter" || arg == "-include" || arg == "-x" || arg == "-MF" || arg == "-MT" || arg == "-Xlinker"
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

		if args[idx] == "-rdynamic" {
			inputPaths = append(inputPaths, "-Wl,--export-dynamic")
			continue
		}

		if args[idx] == "-Xlinker" {
			idx++
			ldExtraArgs = append(ldExtraArgs, args[idx])
			continue
		}

		if args[idx] == "-L" {
			ldExtraArgs = append(ldExtraArgs, "-L")
			idx++
			ldExtraArgs = append(ldExtraArgs, args[idx])
			continue
		}

		if strings.HasPrefix(args[idx], "-L") {
			ldExtraArgs = append(ldExtraArgs, "-L")
			ldExtraArgs = append(ldExtraArgs, args[idx][2:])
			continue
		}

		if args[idx] == "-pthread" {
			define("_REENTRANT")
			inputPaths = append(inputPaths, "-lpthread")
			continue
		}

		if args[idx] == "-static" {
			opt_static = true
			ldExtraArgs = append(ldExtraArgs, "-static")
			continue
		}

		if args[idx] == "-shared" {
			opt_shared = true
			ldExtraArgs = append(ldExtraArgs, "-shared")
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

		if args[idx] == "-P" {
			opt_P = true
			continue
		}

		if args[idx] == "-I" {
			idx++
			includePaths = append(includePaths, args[idx])
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
			inputPaths = append(inputPaths, "-x")
			inputPaths = append(inputPaths, args[idx])
			continue
		}

		if strings.HasPrefix(args[idx], "-x") {
			inputPaths = append(inputPaths, "-x")
			inputPaths = append(inputPaths, args[idx][2:])
			continue
		}

		if strings.HasPrefix(args[idx], "-l") || strings.HasPrefix(args[idx], "-Wl,") {
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

		if args[idx] == "-cc1-asm-pp" {
			opt_E = true
			opt_cc1_asm_pp = true
			continue
		}

		if args[idx] == "-idirafter" {
			idirafter = append(idirafter, args[idx])
			idx++
			continue
		}

		if strings.HasPrefix(args[idx], "-O") {
			if len(args[idx]) > 2 && args[idx][2] == '0' {
				opt_optimize = false
			} else {
				opt_optimize = true
			}
			continue
		}

		if args[idx] == "-ansi" {
			setCStandard(89)
			define("__STRICT_ANSI__")
			continue
		} else if strings.HasPrefix(args[idx], "-std=c") {
			v, _ := strconv.Atoi(args[idx][6:])
			setCStandard(v)
			continue
		} else if strings.HasPrefix(args[idx], "--std=c") {
			v, _ := strconv.Atoi(args[idx][7:])
			setCStandard(v)
			continue
		} else if args[idx] == "--std" {
			idx++
			if args[idx][0] != 'c' {
				panic("unknown c standard")
			}
			v, _ := strconv.Atoi(args[idx][1:])
			setCStandard(v)
			continue
		}

		if strings.HasPrefix(args[idx], "-fstack-reuse=") {
			if args[idx] == "-fstack-reuse=all" {
				DontReuseStack = true
			}
			continue
		}

		if strings.HasPrefix(args[idx], "-g") {
			if len(args[idx]) > 2 && args[idx][2] == '0' {
				opt_g = false
			} else {
				opt_g = true
			}

			continue
		}

		if args[idx] == "-fsigned-char" {
			continue
		}
		if args[idx] == "-funsigned-char" {
			TyPChar.IsUnsigned = true
			continue
		}

		if args[idx] == "-ffunction-sections" {
			opt_func_sections = true
			continue
		}

		if args[idx] == "-fdata-sections" {
			opt_data_sections = true
			continue
		}

		if strings.HasPrefix(args[idx], "-march=") || strings.HasPrefix(args[idx], "-W") || strings.HasPrefix(args[idx], "-std=") || args[idx] == "-ffreestanding" || args[idx] == "-fno-builtin" || args[idx] == "-fno-omit-frame-pointer" || args[idx] == "-fno-stack-protector" || args[idx] == "-fno-strict-aliasing" || args[idx] == "-m64" || args[idx] == "-mno-red-zone" || args[idx] == "-w" || args[idx] == "-fno-lto" || args[idx] == "-fno-asynchronous-unwind-tables" || args[idx] == "-fno-delete-null-pointer-checks" || args[idx] == "-fno-strict-overflow" || args[idx] == "-fwrapv" || args[idx] == "-pedantic" || args[idx] == "-mfpmath=sse" {
			continue
		}

		if strings.HasPrefix(args[idx], "-") && len(args[idx]) > 1 {
			fmt.Fprintf(os.Stderr, "unknown argument: %s\n", args[idx])
			os.Exit(1)
		}

		inputPaths = append(inputPaths, args[idx])
	}

	includePaths = append(includePaths, idirafter...)

	if len(inputPaths) == 0 {
		panic("no input files")
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
		name := files[i].Name
		if (opt_MMD && inStdIncludePath(name)) || name == "zcc_builtins" {
			continue
		}
		fmt.Fprintf(out, " \\\n  %s", name)
	}

	fmt.Fprintf(out, "\n\n")

	if opt_MP {
		for i := 1; i < len(files); i++ {
			name := files[i].Name
			if (opt_MMD && inStdIncludePath(name)) || name == "zcc_builtins" {
				continue
			}
			fmt.Fprintf(out, "%s:\n\n", quoteMakefile(name))
		}
	}
}

func getFileType(filename string) FileType {
	if strings.HasSuffix(filename, ".a") {
		return FILE_AR
	}
	if strings.HasSuffix(filename, ".so") {
		return FILE_DSO
	}
	if strings.HasSuffix(filename, ".o") || strings.HasSuffix(filename, ".lo") {
		return FILE_OBJ
	}

	if strings.HasSuffix(filename, ".c") {
		return FILE_C
	}

	if strings.HasSuffix(filename, ".s") {
		return FILE_ASM
	}

	if strings.HasSuffix(filename, ".S") {
		return FILE_PP_ASM
	}

	if opt_E && filename == "-" {
		return FILE_C
	}

	if strings.Contains(filename, ".so.") {
		p := strings.Index(filename, ".so.")
		p += 4
		for p < len(filename) && (isDecimalDigit(int8(filename[p])) || (filename[p] == '.' && isDecimalDigit(int8(filename[p+1])))) {
			p++
		}

		if p == len(filename) {
			return FILE_DSO
		}
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
	if s == "assembler-with-cpp" {
		return FILE_PP_ASM
	}
	if s == "none" {
		return FILE_NONE
	}
	fmt.Fprintf(os.Stderr, "<command line>: unknown argument for -x: %s\n", s)
	os.Exit(1)
	panic("unreachable")
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

func run_cc1(args []string, input string, output string, option string) {
	args = append(args, "-cc1")

	if input != "" {
		args = append(args, "-cc1-input")
		args = append(args, input)
	}

	if output != "" {
		args = append(args, "-cc1-output")
		args = append(args, output)
	}

	if option != "" {
		args = append(args, option)
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

func mustTokenzieFile(path string, end **Token) *Token {
	tok := tokenizeFile(path, end)
	if tok == nil {
		panic(path)
	}
	return tok
}

func cc1() {
	head := Token{}
	cur := &head

	if !opt_E {
		end := &Token{}
		src := `typedef struct {
                unsigned int gp_offset;
                unsigned int fp_offset;
                void *overflow_arg_area;
                void *reg_save_area;
              } __builtin_va_list[1];`
		srcInInt8 := []int8{}
		for _, b := range src {
			srcInInt8 = append(srcInInt8, int8(b))
		}
		srcInInt8 = append(srcInInt8, 0)
		head.Next = tokenize(addInputFile("zcc_builtins", &srcInInt8), &end)
		cur = end
	}

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

		var end *Token = nil
		cur.Next = mustTokenzieFile(path, &end)
		if end != nil {
			cur = end
		}
	}

	// Tokenize and parse.
	cur.Next = mustTokenzieFile(baseFile, nil)
	tok := preprocess(head.Next)

	// If -M or -MD are given, print file dependencies.
	if opt_M || opt_MD {
		printDependencies()
		if opt_M {
			return
		}
	}

	// If -E is given, print out preprocessed C code as a result.
	if opt_E {
		printTokens(tok, outputFile)
		return
	}

	prog := parse(tok)

	// Traverse the AST to emit assembly.
	if dump_ir {
		println("dump ir not impl")
	} else {
		// Open a temporary output buffer.
		assemblyOutput := []string{}
		codegen(prog, &assemblyOutput)
		out, _ := openFile(outputFile)

		// 使用 bufio.Writer 提高写入效率
		writer := bufio.NewWriter(out)

		for _, line := range assemblyOutput {
			_, err := writer.WriteString(line + "\n")
			if err != nil {
				panic(err)
			}
		}

		// 刷新缓冲区，确保数据写入文件
		err := writer.Flush()
		if err != nil {
			panic(err)
		}
	}
}

// Print tokens to stdout. Used for -E.
func printTokens(tok *Token, path string) {
	var out *os.File
	out, _ = openFile(path)

	line := 1
	var markerFile *File = nil
	for ; tok != nil && tok.Kind != TK_EOF; tok = tok.Next {
		orig := tok
		if tok.Origin != nil {
			orig = tok.Origin
		}
		if !opt_P && markerFile != orig.File {
			markerFile = orig.File
			name := orig.File.Name
			if name == "-" {
				name = "<stdin>"
			}
			fmt.Fprintf(out, "\n# %d \"%s\"\n", orig.LineNo, name)
		}
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
	cmd := []string{"as", input, "-o", output}
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
	path, _ := findFile("/usr/lib*/gcc/x86_64*-linux*/*/crtbegin.o")
	if path != "" {
		return filepath.Dir(path)
	}

	fmt.Fprintf(os.Stderr, "gcc library path is not found")
	os.Exit(1)
	panic("unreachable")
}

func addDefaultIncludePaths(argv0 string) {
	// We expect that chibicc-specific include files are installed
	// to ./include relative to argv[0].
	stdIncludePaths = append(stdIncludePaths, filepath.Dir(argv0)+"/include")

	// Add standard include paths.
	stdIncludePaths = append(stdIncludePaths, "/usr/local/include")
	stdIncludePaths = append(stdIncludePaths, "/usr/include/x86_64-linux-gnu")
	stdIncludePaths = append(stdIncludePaths, "/usr/include")

	includePaths = append(includePaths, stdIncludePaths...)
}

func runLinker(inputs []string, output string) {
	arr := []string{}

	arr = append(arr, "ld")
	arr = append(arr, "-o")
	arr = append(arr, output)
	arr = append(arr, "-m")
	arr = append(arr, "elf_x86_64")

	libpath := findLibPath()
	gccLibpath := findGccLibPath()

	if opt_shared {
		arr = append(arr, libpath+"/crti.o")
		arr = append(arr, gccLibpath+"/crtbeginS.o")
	} else {
		arr = append(arr, libpath+"/crt1.o")
		arr = append(arr, libpath+"/crti.o")
		arr = append(arr, gccLibpath+"/crtbegin.o")
	}
	arr = append(arr, "-L"+gccLibpath)
	arr = append(arr, "-L/usr/lib/x86_64-linux-gnu")
	arr = append(arr, "-L/usr/lib64")
	arr = append(arr, "-L/lib64")
	arr = append(arr, "-L/usr/lib/x86_64-linux-gnu")
	arr = append(arr, "-L/usr/lib/x86_64-pc-linux-gnu")
	arr = append(arr, "-L/usr/lib/x86_64-redhat-linux")
	arr = append(arr, "-L/usr/lib")
	arr = append(arr, "-L/lib")

	if !opt_static {
		arr = append(arr, "-dynamic-linker")
		arr = append(arr, "/lib64/ld-linux-x86-64.so.2")
	}

	arr = append(arr, ldExtraArgs...)
	arr = append(arr, inputs...)

	if opt_static {
		arr = append(arr, "--start-group")
		arr = append(arr, "-lgcc")
		arr = append(arr, "-lgcc_eh")
		arr = append(arr, "-lc")
		arr = append(arr, "--end-group")
	} else {
		arr = append(arr, "-lc")
		arr = append(arr, "-lgcc")
		arr = append(arr, "--as-needed")
		arr = append(arr, "-lgcc_s")
		arr = append(arr, "--no-as-needed")
	}

	if opt_shared {
		arr = append(arr, gccLibpath+"/crtendS.o")
	} else {
		arr = append(arr, gccLibpath+"/crtend.o")
	}
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

	ldArgs := []string{}
	fileCount := 0
	opt_x := FILE_NONE
	run_ld := false

	for i := 0; i < len(inputPaths); i++ {
		if inputPaths[i] == "-x" {
			i++
			opt_x = parseOptX(inputPaths[i])
			continue
		}

		input := inputPaths[i]

		if strings.HasPrefix(input, "-l") {
			ldArgs = append(ldArgs, input)
			continue
		}

		if strings.HasPrefix(input, "-Wl,") {
			s := input[4:]
			args := strings.Split(s, ",")
			ldArgs = append(ldArgs, args...)
			continue
		}

		if opt_o != "" && (opt_c || opt_S || opt_E) {
			fileCount++
			if fileCount > 1 {
				fmt.Fprintf(os.Stderr, "cannot specify '-o' with '-c', '-S' or '-E' with multiple files")
				os.Exit(1)
			}
		}

		output := ""

		if opt_o != "" {
			output = opt_o
		} else if opt_S {
			output = replaceExtension(input, ".s")
		} else {
			output = replaceExtension(input, ".o")
		}

		var filetype FileType
		if opt_x != FILE_NONE {
			filetype = opt_x
		} else {
			filetype = getFileType(input)
		}

		// Handle .o or .a
		if filetype == FILE_OBJ || filetype == FILE_AR || filetype == FILE_DSO {
			ldArgs = append(ldArgs, input)
			run_ld = true
			continue
		}

		// Handle .s
		if filetype == FILE_ASM {
			if opt_S || opt_E || opt_M {
				continue
			}

			if opt_c {
				assemble(input, output)
				continue
			}

			tmp := createTmpfile()
			assemble(input, tmp)
			ldArgs = append(ldArgs, tmp)
			run_ld = true
			continue
		}

		// Handle .S
		if filetype == FILE_PP_ASM {
			if opt_S || opt_E || opt_M {
				if opt_o != "" {
					run_cc1(args, input, opt_o, "-cc1-asm-pp")
				} else {
					run_cc1(args, input, "-", "-cc1-asm-pp")
				}
				continue
			}

			if opt_c {
				tmp := createTmpfile()
				run_cc1(args, input, tmp, "-cc1-asm-pp")
				assemble(tmp, output)
				continue
			}

			tmp1 := createTmpfile()
			tmp2 := createTmpfile()
			run_cc1(args, input, tmp1, "-cc1-asm-pp")
			assemble(tmp1, tmp2)
			ldArgs = append(ldArgs, tmp2)
			run_ld = true
			continue
		}

		if filetype != FILE_C {
			panic("must be .c file")
		}

		// Just preprocess
		if opt_E || opt_M {
			if opt_o != "" {
				run_cc1(args, input, opt_o, "")
			} else {
				run_cc1(args, input, "-", "")
			}
			continue
		}

		// Compile
		if opt_S {
			run_cc1(args, input, output, "")
			continue
		}

		// Compile and assemble
		if opt_c {
			tmp := createTmpfile()
			run_cc1(args, input, tmp, "")
			assemble(tmp, output)
			continue
		}

		// Compile, assemble and link
		tmp1 := createTmpfile()
		tmp2 := createTmpfile()
		run_cc1(args, input, tmp1, "")
		assemble(tmp1, tmp2)
		ldArgs = append(ldArgs, tmp2)
		run_ld = true
		continue
	}

	if run_ld {
		if opt_o != "" {
			runLinker(ldArgs, opt_o)
		} else {
			runLinker(ldArgs, "a.out")
		}
	}
}
