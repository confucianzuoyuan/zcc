use std::{collections::HashMap, sync::Mutex};

use crate::{ast, error, lexer, position, symbol, token};
use lazy_static::lazy_static;

lazy_static! {
    pub static ref VAR_OFFSET_TABLE: Mutex<HashMap<String, i64>> = Mutex::new(HashMap::new());
}

pub fn get_vars() -> Vec<String> {
    let mut _map = VAR_OFFSET_TABLE.lock().unwrap();
    let mut result = vec![];
    for k in _map.keys() {
        result.push(k.clone());
    }
    result
}

pub fn add_var_offset(name: String, offset: i64) {
    let mut _map = VAR_OFFSET_TABLE.lock().unwrap();
    _map.insert(name, offset);
}

pub fn get_var_offset(name: String) -> i64 {
    let mut _map = VAR_OFFSET_TABLE.lock().unwrap();
    *_map.get(&name).unwrap()
}

fn is_var_exist(name: String) -> bool {
    let mut _map = VAR_OFFSET_TABLE.lock().unwrap();
    _map.get(&name).is_some()
}

/// 接收多参数在Rust中实现的方式就是使用宏
macro_rules! eat {
    ($_self:ident, $pat:ident, $var:ident) => {
        match $_self.token() {
            Ok(token) => match token.token {
                $pat(var) => {
                    $var = var;
                    token.pos
                }
                tok => {
                    return Err(error::Error::UnexpectedToken {
                        expected: stringify!($pat).to_lowercase(),
                        pos: token.pos,
                        unexpected: tok,
                    })
                }
            },
            Err(error) => return Err(error),
        }
    };
    ($_self:ident, $pat:ident) => {
        eat!($_self, $pat, stringify!($pat).to_lowercase())
    };
    ($_self:ident, $pat:ident, $expected:expr) => {
        match $_self.token() {
            Ok(token) => match token.token {
                $pat => token.pos,
                tok => {
                    return Err(error::Error::UnexpectedToken {
                        expected: $expected,
                        pos: token.pos,
                        unexpected: tok,
                    })
                }
            },
            Err(error) => return Err(error),
        }
    };
}

pub type Result<T> = std::result::Result<T, error::Error>;

pub struct Parser<'a, R: std::io::Read> {
    lexer: lexer::Lexer<R>,
    lookahead: Option<Result<token::Token>>,
    symbols: &'a mut symbol::Symbols,
}

impl<'a, R: std::io::Read> Parser<'a, R> {
    pub fn new(lexer: lexer::Lexer<R>, symbols: &'a mut symbol::Symbols) -> Self {
        Parser {
            lexer,
            lookahead: None,
            symbols,
        }
    }

    /// 解析表达式时, 首先解析优先级更低的运算符
    /// expr = mul ("+" mul | "-" mul)*
    fn additive_expr(&mut self) -> Result<ast::ExprWithPos> {
        let mut expr = self.multiplicative_expr()?;
        loop {
            let oper = match self.peek_token() {
                Ok(&token::Tok::Minus) => {
                    use token::Tok::Minus;
                    position::WithPos::new(ast::Operator::Minus, eat!(self, Minus))
                }
                Ok(&token::Tok::Plus) => {
                    use token::Tok::Plus;
                    position::WithPos::new(ast::Operator::Plus, eat!(self, Plus))
                }
                _ => break,
            };
            let right = Box::new(self.multiplicative_expr()?);
            let pos = expr.pos.grow(right.pos);
            expr = position::WithPos::new(
                ast::Expr::Binary {
                    left: Box::new(expr),
                    oper,
                    right,
                },
                pos,
            );
        }
        Ok(expr)
    }

    /// mul = primary ("*" primary | "/" primary)*
    fn multiplicative_expr(&mut self) -> Result<ast::ExprWithPos> {
        let mut expr = self.unary_expr()?;
        loop {
            let oper = match self.peek_token() {
                Ok(&token::Tok::Star) => {
                    use token::Tok::Star;
                    position::WithPos::new(ast::Operator::Times, eat!(self, Star))
                }
                Ok(&token::Tok::Slash) => {
                    use token::Tok::Slash;
                    position::WithPos::new(ast::Operator::Divide, eat!(self, Slash))
                }
                _ => break,
            };
            let right = Box::new(self.unary_expr()?);
            let pos = expr.pos.grow(right.pos);
            expr = position::WithPos::new(
                ast::Expr::Binary {
                    left: Box::new(expr),
                    oper,
                    right,
                },
                pos,
            );
        }
        Ok(expr)
    }

    /// unary = ("+" | "-") unary
    ///       | primary
    fn unary_expr(&mut self) -> Result<ast::ExprWithPos> {
        match self.peek()?.token {
            token::Tok::Plus => {
                use token::Tok::Plus;
                eat!(self, Plus);
                self.unary_expr()
            }
            token::Tok::Minus => {
                use token::Tok::Minus;
                let pos = eat!(self, Minus);
                let expr = self.unary_expr()?;
                let pos = pos.grow(expr.pos);
                Ok(position::WithPos::new(
                    ast::Expr::Unary {
                        oper: position::WithPos::new(ast::Operator::Minus, pos),
                        expr: Box::new(expr),
                    },
                    pos,
                ))
            }
            _ => self.primary_expr(),
        }
    }

    // primary = "(" expr ")" | num
    fn primary_expr(&mut self) -> Result<ast::ExprWithPos> {
        match self.peek()?.token {
            token::Tok::Number(_) => self.int_lit(),
            token::Tok::OpenParen => self.seq_exp(),
            token::Tok::Ident(_) => {
                let name;
                use token::Tok::Ident;
                let pos = eat!(self, Ident, name);
                if is_var_exist(name.clone()) == false {
                    add_var_offset(name.clone(), 0);
                }
                Ok(ast::ExprWithPos::new(
                    ast::Expr::Var(ast::VarObj { name, offset: 0 }),
                    pos,
                ))
            }
            _ => Err(self.unexpected_token("integer literal, (")?),
        }
    }

    /// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
    fn relational_expr(&mut self) -> Result<ast::ExprWithPos> {
        let mut expr = self.additive_expr()?;
        loop {
            let oper = match self.peek_token() {
                Ok(&token::Tok::LesserThan) => {
                    use token::Tok::LesserThan;
                    position::WithPos::new(ast::Operator::LesserThan, eat!(self, LesserThan))
                }
                Ok(&token::Tok::LesserOrEqual) => {
                    use token::Tok::LesserOrEqual;
                    position::WithPos::new(ast::Operator::LesserOrEqual, eat!(self, LesserOrEqual))
                }
                Ok(&token::Tok::GreaterThan) => {
                    use token::Tok::GreaterThan;
                    position::WithPos::new(ast::Operator::GreaterThan, eat!(self, GreaterThan))
                }
                Ok(&token::Tok::GreaterOrEqual) => {
                    use token::Tok::GreaterOrEqual;
                    position::WithPos::new(
                        ast::Operator::GreaterOrEqual,
                        eat!(self, GreaterOrEqual),
                    )
                }
                _ => break,
            };
            let right = Box::new(self.additive_expr()?);
            let pos = expr.pos.grow(right.pos);
            expr = position::WithPos::new(
                ast::Expr::Binary {
                    left: Box::new(expr),
                    oper,
                    right,
                },
                pos,
            );
        }
        Ok(expr)
    }

    /// equality = relational ("==" relational | "!=" relational)*
    fn equality_expr(&mut self) -> Result<ast::ExprWithPos> {
        let mut expr = self.relational_expr()?;
        loop {
            let oper = match self.peek_token() {
                Ok(&token::Tok::DoubleEqual) => {
                    use token::Tok::DoubleEqual;
                    position::WithPos::new(ast::Operator::Equal, eat!(self, DoubleEqual))
                }
                Ok(&token::Tok::BangEqual) => {
                    use token::Tok::BangEqual;
                    position::WithPos::new(ast::Operator::NotEqual, eat!(self, BangEqual))
                }
                _ => break,
            };
            let right = Box::new(self.relational_expr()?);
            let pos = expr.pos.grow(right.pos);
            expr = position::WithPos::new(
                ast::Expr::Binary {
                    left: Box::new(expr),
                    oper,
                    right,
                },
                pos,
            );
        }
        Ok(expr)
    }

    fn seq_exp(&mut self) -> Result<ast::ExprWithPos> {
        use token::Tok::OpenParen;
        eat!(self, OpenParen);
        let expr = self.expr()?;
        use token::Tok::CloseParen;
        eat!(self, CloseParen);
        Ok(expr)
    }

    fn int_lit(&mut self) -> Result<ast::ExprWithPos> {
        let value;
        use token::Tok::Number;
        let pos = eat!(self, Number, value);
        Ok(position::WithPos::new(ast::Expr::Int { value }, pos))
    }

    fn expr(&mut self) -> Result<ast::ExprWithPos> {
        self.assign_expr()
    }

    /// assign = equality ("=" assign)?
    fn assign_expr(&mut self) -> Result<ast::ExprWithPos> {
        let lvalue = self.equality_expr()?;
        match self.peek()?.token {
            token::Tok::Equal => {
                use token::Tok::Equal;
                eat!(self, Equal);
                Ok(position::WithPos::new(
                    ast::Expr::Assign {
                        lvalue: Box::new(lvalue.clone()),
                        rvalue: Box::new(self.assign_expr()?),
                    },
                    lvalue.pos,
                ))
            }
            _ => Ok(lvalue),
        }
    }

    /// expr-stmt = expr ";"
    fn expr_stmt(&mut self) -> Result<ast::StmtWithPos> {
        let expr = self.expr()?;
        use token::Tok::Semicolon;
        eat!(self, Semicolon);
        let stmt = ast::StmtWithPos {
            node: ast::Stmt::Expr(expr.clone()),
            pos: expr.pos,
        };
        Ok(stmt)
    }

    /// stmt = "return" expr ";"
    ///      | expr-stmt
    fn stmt(&mut self) -> Result<ast::StmtWithPos> {
        match self.peek()?.token {
            token::Tok::Return => {
                use token::Tok::{Return, Semicolon};
                eat!(self, Return);
                let e = self.expr()?;
                eat!(self, Semicolon);
                let stmt = ast::StmtWithPos {
                    node: ast::Stmt::Return(e.clone()),
                    pos: e.pos,
                };
                Ok(stmt)
            }
            _ => self.expr_stmt(),
        }
    }

    fn peek(&mut self) -> std::result::Result<&token::Token, &error::Error> {
        if self.lookahead.is_none() {
            self.lookahead = Some(self.lexer.token());
        }

        // NOTE: lookahead always contain a value, hence unwrap
        self.lookahead.as_ref().unwrap().as_ref()
    }

    fn peek_token(&mut self) -> std::result::Result<&token::Tok, &error::Error> {
        self.peek().map(|token| &token.token)
    }

    fn token(&mut self) -> Result<token::Token> {
        if let Some(token) = self.lookahead.take() {
            return token;
        }
        self.lexer.token()
    }

    fn unexpected_token(&mut self, expected: &str) -> Result<error::Error> {
        let token = self.token()?;
        Err(error::Error::UnexpectedToken {
            expected: expected.to_string(),
            pos: token.pos,
            unexpected: token.token,
        })
    }

    pub fn parse(&mut self) -> Result<ast::Function> {
        let mut stmts = vec![];
        loop {
            if self.peek_token()? == &token::Tok::EndOfFile {
                break;
            }
            stmts.push(self.stmt()?);
        }
        match self.token() {
            Ok(token::Token {
                token: token::Tok::EndOfFile,
                ..
            })
            | Err(error::Error::Eof) => Ok(ast::Function {
                body: stmts,
                stack_size: 0,
            }),
            _ => Err(self.unexpected_token("end of file")?),
        }
    }
}
