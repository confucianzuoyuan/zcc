use std::fmt::Display;

#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord)]
pub enum TokenKind {
    Plus,                // "+"
    PlusEqual,           // "+="
    Minus,               // "-"
    MinusEqual,          // "-="
    Star,                // "*"
    StarEqual,           // "*="
    Slash,               // "/"
    SlashEqual,          // "/="
    And,                 // "&"
    AndAnd,              // "&&"
    AndEqual,            // "&="
    Or,                  // "|"
    OrOr,                // "||"
    OrEqual,             // "|="
    Sharp,               // "#"
    SharpSharp,          // "##"
    HatEqual,            // "^="
    OpenParen,           // "("
    CloseParen,          // ")"
    OpenBrace,           // "{"
    CloseBrace,          // "}"
    OpenBracket,         // "["
    CloseBracket,        // "]"
    Equal,               // "="
    EqualEqual,          // "=="
    Bang,                // "!"
    BangEqual,           // "!="
    LessThan,            // "<"
    LessOrEqual,         // "<="
    GreaterThan,         // ">"
    GreaterOrEqual,      // ">="
    PlusPlus,            // "++"
    MinusMinus,          // "--"
    LessLess,            // "<<"
    LessLessEqual,       // "<<="
    GreaterGreater,      // ">>"
    GreaterGreaterEqual, // ">>="
    Semicolon,           // ";"
    Colon,               // ":"
    QuestionMark,        // "?"
    Comma,               // ","
    Dot,                 // "."
    DotDotDot,           // "..."
    MinusGreater,        // "->"
    Percent,             // "%"
    PercentEqual,        // "%="
    BackSlash,           // "\"
    Number(i64),
    Ident(String),
    String(String),

    // 关键字
    KWReturn,                          // "return"
    KWIf,                              // "if"
    KWElse,                            // "else"
    KWFor,                             // "for"
    KWWhile,                           // "while"
    KWInt,                             // "int"
    KWChar,                            // "char"
    KWUnsigned,                        // "unsigned"
    KWDo,                              // "do"
    KWSwitch,                          // "switch"
    KWCase,                            // "case"
    KWSizeOf,                          // "sizeof"
    KWRestrict,                        // "restrict"
    KWUnderscoreRestrict,              // "__restrict",
    KWUnderscoreRestrictUnderscore,    // "__restrict__"
    KWUnderscoreNoReturn,              // "_Noreturn"
    KWFloat,                           // "float"
    KWDouble,                          // "double",
    KWLong,                            // "long"
    KWVolatile,                        // "volatile"
    KWRegister,                        // "register"
    KWTypeOf,                          // "typeof"
    KWAsm,                             // "asm"
    KWSigned,                          // "signed"
    KWConst,                           // "const"
    KWDefault,                         // "default"
    KWGoto,                            // "goto"
    KWBreak,                           // "break"
    KWShort,                           // "short"
    KWVoid,                            // "void"
    KWTypeDef,                         // "typedef"
    KWExtern,                          // "extern"
    KWStatic,                          // "static"
    KWEnum,                            // "enum"
    KWUnion,                           // "union"
    KWUnderscoreBool,                  // "_Bool"
    KWUnderscoreAlignas,               // "_Alignas"
    KWUnderscoreAlignof,               // "_Alignof"
    KWAuto,                            // "auto"
    KWUnderscoreThreadUnderscoreLocal, // "_Thread_local"
    KWUnderscoreThread,                // "__thread"
    KWUnderscoreAtomic,                // "_Atomic"
    KWUnderscoreAttributeUnderscore,   // "__attribute__"
    KWContinue,                        // "continue"
    KWStruct,                          // "struct"

    // 文件结束符
    EndOfFile,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub struct Token {
    pub loc: usize,
    pub len: usize,
    pub line_no: usize,
    pub token: TokenKind,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = (|| {
            let string = match *self {
                TokenKind::Plus => "+",
                TokenKind::PlusEqual => "+=",
                TokenKind::MinusEqual => "-=",
                TokenKind::Minus => "-",
                TokenKind::Slash => "/",
                TokenKind::Star => "*",
                TokenKind::And => "&",
                TokenKind::Equal => "=",
                TokenKind::EqualEqual => "==",
                TokenKind::Bang => "!",
                TokenKind::BangEqual => "!=",
                TokenKind::LessOrEqual => "<=",
                TokenKind::LessThan => "<",
                TokenKind::GreaterOrEqual => ">=",
                TokenKind::GreaterThan => ">",
                TokenKind::OpenParen => "(",
                TokenKind::CloseParen => ")",
                TokenKind::OpenBrace => "{",
                TokenKind::CloseBrace => "}",
                TokenKind::OpenBracket => "[",
                TokenKind::CloseBracket => "]",
                TokenKind::Semicolon => ";",
                TokenKind::Colon => ":",
                TokenKind::QuestionMark => "?",
                TokenKind::Comma => ",",
                TokenKind::AndAnd => "&&",
                TokenKind::Dot => ".",
                TokenKind::DotDotDot => "...",
                TokenKind::GreaterGreater => ">>",
                TokenKind::LessLess => "<<",
                TokenKind::AndEqual => "&=",
                TokenKind::GreaterGreaterEqual => ">>=",
                TokenKind::HatEqual => "^=",
                TokenKind::LessLessEqual => "<<=",
                TokenKind::MinusGreater => "->",
                TokenKind::MinusMinus => "--",
                TokenKind::OrOr => "||",
                TokenKind::Percent => "%",
                TokenKind::PercentEqual => "%=",
                TokenKind::PlusPlus => "++",
                TokenKind::Sharp => "#",
                TokenKind::SharpSharp => "##",
                TokenKind::SlashEqual => "/=",
                TokenKind::StarEqual => "*=",
                TokenKind::Or => "|",
                TokenKind::OrEqual => "|=",
                TokenKind::BackSlash => "\\",
                TokenKind::KWReturn => "return",
                TokenKind::KWIf => "if",
                TokenKind::KWElse => "else",
                TokenKind::KWFor => "for",
                TokenKind::KWWhile => "while",
                TokenKind::KWInt => "int",
                TokenKind::KWChar => "char",
                TokenKind::KWSizeOf => "sizeof",
                TokenKind::KWDo => "do",
                TokenKind::KWSwitch => "switch",
                TokenKind::KWCase => "case",
                TokenKind::KWUnsigned => "unsigned",
                TokenKind::KWAsm => "asm",
                TokenKind::KWAuto => "auto",
                TokenKind::KWBreak => "break",
                TokenKind::KWConst => "const",
                TokenKind::KWDefault => "default",
                TokenKind::KWDouble => "double",
                TokenKind::KWEnum => "enum",
                TokenKind::KWExtern => "extern",
                TokenKind::KWFloat => "float",
                TokenKind::KWGoto => "goto",
                TokenKind::KWLong => "long",
                TokenKind::KWRegister => "register",
                TokenKind::KWRestrict => "restrict",
                TokenKind::KWShort => "short",
                TokenKind::KWSigned => "signed",
                TokenKind::KWStatic => "static",
                TokenKind::KWTypeDef => "typedef",
                TokenKind::KWTypeOf => "typeof",
                TokenKind::KWUnderscoreAlignas => "_Alignas",
                TokenKind::KWUnderscoreAlignof => "_Alignof",
                TokenKind::KWUnderscoreAtomic => "_Atomic",
                TokenKind::KWUnderscoreAttributeUnderscore => "__attribute__",
                TokenKind::KWUnderscoreBool => "_Bool",
                TokenKind::KWUnderscoreNoReturn => "_Noreturn",
                TokenKind::KWUnderscoreRestrict => "__restrict",
                TokenKind::KWUnderscoreRestrictUnderscore => "__restrict__",
                TokenKind::KWUnderscoreThread => "__thread",
                TokenKind::KWUnderscoreThreadUnderscoreLocal => "_Thread_local",
                TokenKind::KWUnion => "union",
                TokenKind::KWVoid => "void",
                TokenKind::KWVolatile => "volatile",
                TokenKind::KWContinue => "continue",
                TokenKind::KWStruct => "struct",
                TokenKind::Number(num) => return num.to_string(),
                TokenKind::String(ref string) => return format!("{:?}", string),
                TokenKind::Ident(ref ident) => ident,
                TokenKind::EndOfFile => "<eof>",
            };
            string.to_string()
        })();
        write!(f, "{}", string)
    }
}
