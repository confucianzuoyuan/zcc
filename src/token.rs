use std::fmt::Display;

use crate::position;

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
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
    Comma,               // ","
    DotDotDot,           // "..."
    MinusGreater,        // "->"
    Percent,             // "%"
    PercentEqual,        // "%="
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

    // 文件结束符
    EndOfFile,
}

#[derive(Debug)]
pub struct Token {
    pub pos: position::Pos,
    pub token: Tok,
}

impl Display for Tok {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = (|| {
            let string = match *self {
                Tok::Plus => "+",
                Tok::PlusEqual => "+=",
                Tok::MinusEqual => "-=",
                Tok::Minus => "-",
                Tok::Slash => "/",
                Tok::Star => "*",
                Tok::And => "&",
                Tok::Equal => "=",
                Tok::EqualEqual => "==",
                Tok::Bang => "!",
                Tok::BangEqual => "!=",
                Tok::LessOrEqual => "<=",
                Tok::LessThan => "<",
                Tok::GreaterOrEqual => ">=",
                Tok::GreaterThan => ">",
                Tok::OpenParen => "(",
                Tok::CloseParen => ")",
                Tok::OpenBrace => "{",
                Tok::CloseBrace => "}",
                Tok::OpenBracket => "[",
                Tok::CloseBracket => "]",
                Tok::Semicolon => ";",
                Tok::Comma => ",",
                Tok::AndAnd => "&&",
                Tok::DotDotDot => "...",
                Tok::GreaterGreater => ">>",
                Tok::LessLess => "<<",
                Tok::AndEqual => "&=",
                Tok::GreaterGreaterEqual => ">>=",
                Tok::HatEqual => "^=",
                Tok::LessLessEqual => "<<=",
                Tok::MinusGreater => "->",
                Tok::MinusMinus => "--",
                Tok::OrOr => "||",
                Tok::Percent => "%",
                Tok::PercentEqual => "%=",
                Tok::PlusPlus => "++",
                Tok::SharpSharp => "##",
                Tok::SlashEqual => "/=",
                Tok::StarEqual => "*=",
                Tok::Or => "|",
                Tok::OrEqual => "|=",
                Tok::KWReturn => "return",
                Tok::KWIf => "if",
                Tok::KWElse => "else",
                Tok::KWFor => "for",
                Tok::KWWhile => "while",
                Tok::KWInt => "int",
                Tok::KWChar => "char",
                Tok::KWSizeOf => "sizeof",
                Tok::KWDo => "do",
                Tok::KWSwitch => "switch",
                Tok::KWCase => "case",
                Tok::KWUnsigned => "unsigned",
                Tok::KWAsm => "asm",
                Tok::KWAuto => "auto",
                Tok::KWBreak => "break",
                Tok::KWConst => "const",
                Tok::KWDefault => "default",
                Tok::KWDouble => "double",
                Tok::KWEnum => "enum",
                Tok::KWExtern => "extern",
                Tok::KWFloat => "float",
                Tok::KWGoto => "goto",
                Tok::KWLong => "long",
                Tok::KWRegister => "register",
                Tok::KWRestrict => "restrict",
                Tok::KWShort => "short",
                Tok::KWSigned => "signed",
                Tok::KWStatic => "static",
                Tok::KWTypeDef => "typedef",
                Tok::KWTypeOf => "typeof",
                Tok::KWUnderscoreAlignas => "_Alignas",
                Tok::KWUnderscoreAlignof => "_Alignof",
                Tok::KWUnderscoreAtomic => "_Atomic",
                Tok::KWUnderscoreAttributeUnderscore => "__attribute__",
                Tok::KWUnderscoreBool => "_Bool",
                Tok::KWUnderscoreNoReturn => "_Noreturn",
                Tok::KWUnderscoreRestrict => "__restrict",
                Tok::KWUnderscoreRestrictUnderscore => "__restrict__",
                Tok::KWUnderscoreThread => "__thread",
                Tok::KWUnderscoreThreadUnderscoreLocal => "_Thread_local",
                Tok::KWUnion => "union",
                Tok::KWVoid => "void",
                Tok::KWVolatile => "volatile",
                Tok::Number(num) => return num.to_string(),
                Tok::String(ref string) => return format!("{:?}", string),
                Tok::Ident(ref ident) => ident,
                Tok::EndOfFile => "<eof>",
            };
            string.to_string()
        })();
        write!(f, "{}", string)
    }
}
