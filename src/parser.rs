use crate::{ast, constant, error, token, types};

pub type Result<T> = std::result::Result<T, error::Error>;

#[derive(Debug)]
pub enum Declarator {
    Ident(String),
    PointerDeclarator(Box<Declarator>),
    ArrayDeclarator(Box<Declarator>, constant::Type),
    FunDeclarator(Vec<ParamInfo>, Box<Declarator>),
}

pub type ParamInfo = (types::Type, Declarator);

pub struct Parser {
    pub tokens: Vec<token::Token>,
    pub current_pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<token::Token>) -> Self {
        Self {
            tokens,
            current_pos: 0,
        }
    }

    fn advance(&mut self) {
        self.current_pos += 1;
    }

    fn expect(&mut self, expected: token::TokenKind) -> Result<()> {
        let actual = self.current_token();
        if actual.token == expected {
            self.advance();
            Ok(())
        } else {
            Err(error::Error {
                line_number: actual.line_no,
                location: actual.loc,
                message: format!("Expected {} but found {}", expected, actual.token),
            })
        }
    }

    /// 查看当前 token
    fn current_token(&self) -> token::Token {
        self.tokens[self.current_pos].clone()
    }

    /// 查看当前 token 并消费掉
    fn next_token(&mut self) -> token::Token {
        let next_token = self.tokens[self.current_pos].clone();
        self.advance();
        next_token
    }

    fn parse_id(&mut self) -> Result<String> {
        let token = self.current_token();
        self.advance();
        match token {
            token::Token {
                token: token::TokenKind::Ident(x),
                ..
            } => Ok(x),
            other => Err(error::Error {
                line_number: other.line_no,
                location: other.loc,
                message: format!("Expected an identifier, but found {}", other.token),
            }),
        }
    }

    fn is_type_specifier(&self, t: token::Token) -> bool {
        use token::TokenKind::*;
        match t.token {
            KWInt | KWLong | KWUnsigned | KWSigned | KWDouble | KWChar | KWVoid | KWStruct => true,
            _ => false,
        }
    }

    fn is_specifier(&self, t: token::Token) -> bool {
        use token::TokenKind::*;
        match t.token {
            KWStatic | KWExtern => true,
            _ => self.is_type_specifier(t),
        }
    }

    fn parse_type_specifier(&mut self) -> Result<token::Token> {
        match self.current_token() {
            token::Token {
                token: token::TokenKind::KWStruct,
                ..
            } => {
                self.advance();
                match self.next_token() {
                    t @ token::Token {
                        token: token::TokenKind::Ident(..),
                        ..
                    } => Ok(t),
                    other => Err(error::Error {
                        line_number: self.current_token().line_no,
                        location: self.current_token().loc,
                        message: format!("Expected a structure tag, but found {}", other.token),
                    }),
                }
            }
            t if self.is_type_specifier(t.clone()) => {
                self.advance();
                Ok(t)
            }
            t => Err(error::Error {
                line_number: t.line_no,
                location: t.loc,
                message: format!(
                    "Internal error: called parse_type_specifier on non-type specifier token: {}",
                    t.token
                ),
            }),
        }
    }

    fn parse_specifier(&mut self) -> Result<token::Token> {
        use token::TokenKind::*;
        let t = self.current_token();
        match t.token {
            KWStatic | KWExtern => {
                self.advance();
                Ok(t)
            }
            _ => self.parse_type_specifier(),
        }
    }

    fn parse_type_specifier_list(&mut self) -> Result<Vec<token::Token>> {
        if self.is_type_specifier(self.current_token()) {
            let spec = self.parse_type_specifier()?;
            let mut specs = vec![];
            specs.push(spec);
            specs.append(&mut self.parse_type_specifier_list()?);
            Ok(specs)
        } else {
            Ok(vec![])
        }
    }

    fn parse_storage_class(&mut self, t: token::Token) -> Result<ast::StorageClass> {
        use token::TokenKind::*;
        match t.token {
            KWExtern => Ok(ast::StorageClass::Extern),
            KWStatic => Ok(ast::StorageClass::Static),
            _ => Err(error::Error {
                line_number: t.line_no,
                location: t.loc,
                message: "Internal error: bad storage class".to_string(),
            }),
        }
    }

    fn parse_type(&mut self, mut specifier_list: Vec<token::Token>) -> Result<types::Type> {
        specifier_list.sort();
        match &specifier_list[..] {
            [token::Token {
                token: token::TokenKind::Ident(tag),
                ..
            }] => Ok(types::Type::Structure(tag.to_string())),
            [token::Token {
                token: token::TokenKind::KWVoid,
                ..
            }] => Ok(types::Type::Void),
            [token::Token {
                token: token::TokenKind::KWDouble,
                ..
            }] => Ok(types::Type::Double),
            [token::Token {
                token: token::TokenKind::KWChar,
                ..
            }] => Ok(types::Type::Char),
            [token::Token {
                token: token::TokenKind::KWChar,
                ..
            }, token::Token {
                token: token::TokenKind::KWSigned,
                ..
            }] => Ok(types::Type::SChar),
            [token::Token {
                token: token::TokenKind::KWChar,
                ..
            }, token::Token {
                token: token::TokenKind::KWUnsigned,
                ..
            }] => Ok(types::Type::UChar),
            _ => {
                use token::TokenKind::*;
                if specifier_list.is_empty()
                    || specifier_list
                        .iter()
                        .find(|t| t.token == KWDouble)
                        .is_some()
                    || specifier_list.iter().find(|t| t.token == KWChar).is_some()
                    || specifier_list.iter().find(|t| t.token == KWVoid).is_some()
                    || specifier_list
                        .iter()
                        .find(|t| match t.token {
                            Ident(..) => true,
                            _ => false,
                        })
                        .is_some()
                    || (specifier_list
                        .iter()
                        .find(|t| t.token == KWUnsigned)
                        .is_some()
                        && specifier_list
                            .iter()
                            .find(|t| t.token == KWSigned)
                            .is_some())
                {
                    return Err(error::Error {
                        line_number: self.current_token().line_no,
                        location: self.current_token().loc,
                        message: "Invalid type specifier".to_string(),
                    });
                } else if specifier_list
                    .iter()
                    .find(|t| t.token == KWUnsigned)
                    .is_some()
                    || specifier_list.iter().find(|t| t.token == KWLong).is_some()
                {
                    Ok(types::Type::ULong)
                } else if specifier_list
                    .iter()
                    .find(|t| t.token == KWUnsigned)
                    .is_some()
                {
                    Ok(types::Type::UInt)
                } else if specifier_list.iter().find(|t| t.token == KWLong).is_some() {
                    Ok(types::Type::Long)
                } else {
                    Ok(types::Type::Int)
                }
            }
        }
    }

    fn parse_type_and_storage_class(
        &mut self,
        specifier_list: Vec<token::Token>,
    ) -> Result<(types::Type, Option<ast::StorageClass>)> {
        use token::TokenKind::*;
        let (storage_classes, types): (Vec<_>, Vec<_>) = specifier_list
            .iter()
            .partition(|&t| t.token == KWExtern || t.token == KWStatic);
        let typ = self.parse_type(types.iter().map(|&t| t.clone()).collect())?;
        match storage_classes[..] {
            [] => Ok((typ, None)),
            [sc] => Ok((typ, Some(self.parse_storage_class(sc.clone())?))),
            _ => Err(error::Error {
                line_number: self.current_token().line_no,
                location: self.current_token().loc,
                message: "Invalid storage class".to_string(),
            }),
        }
    }

    fn parse_declarator(&mut self) -> Result<Declarator> {
        match self.current_token().token {
            token::TokenKind::Star => {
                self.advance();
                let inner = self.parse_declarator()?;
                Ok(Declarator::PointerDeclarator(Box::new(inner)))
            }
            _ => self.parse_direct_declarator(),
        }
    }

    fn parse_simple_declarator(&mut self) -> Result<Declarator> {
        let next_token = self.next_token();
        match next_token.token {
            token::TokenKind::OpenParen => {
                let decl = self.parse_declarator()?;
                self.expect(token::TokenKind::CloseParen)?;
                Ok(decl)
            }
            token::TokenKind::Ident(id) => Ok(Declarator::Ident(id)),
            _ => Err(error::Error {
                line_number: next_token.line_no,
                location: next_token.loc,
                message: format!(
                    "Expected a simple declarator but found {}",
                    next_token.token
                ),
            }),
        }
    }

    fn parse_constant(&mut self) -> Result<constant::Type> {
        let next_token = self.next_token();
        match next_token.token {
            token::TokenKind::Number(i) => Ok(constant::Type::ConstInt(i as i32)),
            _ => Err(error::Error {
                line_number: next_token.line_no,
                location: next_token.loc,
                message: format!("Expected a constant, but found {}", next_token.token),
            }),
        }
    }

    fn parse_array_dimensions(&mut self) -> Result<Vec<constant::Type>> {
        match self.current_token().token {
            token::TokenKind::OpenBracket => {
                self.advance();
                let dim = self.parse_constant()?;
                self.expect(token::TokenKind::CloseBracket)?;
                let mut dims = vec![];
                dims.push(dim);
                dims.append(&mut self.parse_array_dimensions()?);
                Ok(dims)
            }
            _ => Ok(vec![]),
        }
    }

    fn parse_direct_declarator(&mut self) -> Result<Declarator> {
        use token::TokenKind::*;
        let mut simple_dec = self.parse_simple_declarator()?;
        match self.current_token().token {
            OpenBracket => {
                let array_dimensions = self.parse_array_dimensions()?;
                for dim in array_dimensions {
                    simple_dec = Declarator::ArrayDeclarator(Box::new(simple_dec), dim);
                }
                Ok(simple_dec)
            }
            OpenParen => {
                let params = self.parse_param_list()?;
                Ok(Declarator::FunDeclarator(params, Box::new(simple_dec)))
            }
            _ => Ok(simple_dec),
        }
    }

    fn parse_param_list(&mut self) -> Result<Vec<ParamInfo>> {
        use token::TokenKind::*;
        self.expect(OpenParen)?;
        let params = match self
            .tokens
            .iter()
            .map(|t| t.token.clone())
            .collect::<Vec<_>>()[self.current_pos..]
        {
            [KWVoid, CloseParen] => {
                self.advance();
                Ok(vec![])
            }
            _ => self.param_loop(),
        };
        self.expect(CloseParen)?;
        params
    }

    fn param_loop(&mut self) -> Result<Vec<ParamInfo>> {
        let p = self.parse_param()?;
        match self.current_token().token {
            token::TokenKind::Comma => {
                self.advance();
                let mut ps = vec![];
                ps.push(p);
                ps.append(&mut self.param_loop()?);
                Ok(ps)
            }
            _ => Ok(vec![p]),
        }
    }

    fn parse_param(&mut self) -> Result<ParamInfo> {
        let specifiers = self.parse_type_specifier_list()?;
        let param_type = self.parse_type(specifiers)?;
        let param_decl = self.parse_declarator()?;
        Ok((param_type, param_decl))
    }

    fn process_declarator(
        &mut self,
        decl: Declarator,
        base_type: types::Type,
    ) -> Result<(String, types::Type, Vec<String>)> {
        match decl {
            Declarator::Ident(s) => Ok((s, base_type, vec![])),
            Declarator::PointerDeclarator(d) => {
                let derived_type = types::Type::Pointer(Box::new(base_type));
                self.process_declarator(*d, derived_type)
            }
            Declarator::ArrayDeclarator(inner, cnst) => {
                let size = constant::const_to_dim(cnst);
                let derived_type = types::Type::Array {
                    elem_type: Box::new(base_type),
                    size,
                };
                self.process_declarator(*inner, derived_type)
            }
            Declarator::FunDeclarator(params, d) => match *d {
                Declarator::Ident(s) => {
                    let mut param_names = vec![];
                    let mut param_types = vec![];
                    for (p_base_type, p_decl) in params {
                        let (param_name, param_t, _) =
                            self.process_declarator(p_decl, p_base_type)?;
                        match param_t {
                            types::Type::FunType { .. } => {
                                return Err(error::Error {
                                    line_number: self.current_token().line_no,
                                    location: self.current_token().loc,
                                    message: "Function pointers in parameters are not supported"
                                        .to_string(),
                                })
                            }
                            _ => (),
                        }
                        param_names.push(param_name);
                        param_types.push(param_t);
                    }
                    let fun_type = types::Type::FunType {
                        param_types,
                        ret_type: Box::new(base_type),
                    };
                    Ok((s, fun_type, param_names))
                }
                _ => Err(error::Error {
                    line_number: self.current_token().line_no,
                    location: self.current_token().loc,
                    message: "cannot apply additional type derivations to a function declarator"
                        .to_string(),
                }),
            },
        }
    }

    fn parse_member(&mut self) -> Result<ast::MemberDeclaration> {
        let specifiers = self.parse_type_specifier_list()?;
        let t = self.parse_type(specifiers)?;
        let member_decl = self.parse_declarator()?;
        match member_decl {
            Declarator::FunDeclarator(..) => Err(error::Error {
                line_number: self.current_token().line_no,
                location: self.current_token().loc,
                message: "found function declarator in struct member list".to_string(),
            }),
            _ => {
                self.expect(token::TokenKind::Semicolon)?;
                let (member_name, member_type, _) = self.process_declarator(member_decl, t)?;
                Ok(ast::MemberDeclaration {
                    member_name,
                    member_type,
                })
            }
        }
    }

    fn parse_member_list(&mut self) -> Result<Vec<ast::MemberDeclaration>> {
        let m = self.parse_member()?;
        match self.current_token().token {
            token::TokenKind::CloseBrace => Ok(vec![m]),
            _ => {
                let mut m_list = vec![];
                m_list.push(m);
                m_list.append(&mut self.parse_member_list()?);
                Ok(m_list)
            }
        }
    }

    fn parse_structure_declaration(
        &mut self,
    ) -> Result<ast::Declaration<ast::UntypedInitializer, ast::UntypedExp>> {
        self.expect(token::TokenKind::KWStruct)?;
        let tag = self.parse_id()?;
        let members = match self.next_token() {
            token::Token {
                token: token::TokenKind::Semicolon,
                ..
            } => vec![],
            token::Token {
                token: token::TokenKind::OpenBrace,
                ..
            } => {
                let members = self.parse_member_list()?;
                self.expect(token::TokenKind::CloseBrace)?;
                self.expect(token::TokenKind::Semicolon)?;
                members
            }
            _ => {
                return Err(error::Error {
                    line_number: self.current_token().line_no,
                    location: self.current_token().loc,
                    message: format!("Unexpected {}", self.current_token().token),
                })
            }
        };
        Ok(ast::Declaration::StructDecl(ast::StructDeclaration {
            tag,
            members,
        }))
    }

    fn parse_declaration(
        &mut self,
    ) -> Result<ast::Declaration<ast::UntypedInitializer, ast::UntypedExp>> {
        match self.tokens[self.current_pos..] {
            [token::Token {
                token: token::TokenKind::KWStruct,
                ..
            }, token::Token {
                token: token::TokenKind::Ident(..),
                ..
            }, token::Token {
                token: token::TokenKind::OpenBrace,
                ..
            }
            | token::Token {
                token: token::TokenKind::Semicolon,
                ..
            }, ..] => self.parse_structure_declaration(),
            _ => Err(error::Error {
                line_number: self.current_token().line_no,
                location: self.current_token().loc,
                message: "struct definition syntax wrong".to_string(),
            }),
        }
    }

    fn parse_declaration_list(
        &mut self,
    ) -> Result<Vec<ast::Declaration<ast::UntypedInitializer, ast::UntypedExp>>> {
        match self.current_token().token {
            token::TokenKind::EndOfFile => Ok(vec![]),
            _ => {
                let mut decls = vec![];
                let next_decl = self.parse_declaration()?;
                decls.push(next_decl);
                decls.append(&mut self.parse_declaration_list()?);
                Ok(decls)
            }
        }
    }

    pub fn parse(&mut self) -> Result<ast::UntypedProgram> {
        self.parse_declaration_list()
    }
}
