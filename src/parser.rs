use std::{collections::HashMap, sync::Mutex};

use crate::{ast, error, lexer, position, symbol, token, types};

pub static mut VAR_VEC: Vec<ast::VarObj> = vec![];

pub fn get_vars() -> Vec<String> {
    let mut result = vec![];
    unsafe {
        for k in &VAR_VEC {
            result.push(k.name.clone());
        }
    }
    result
}

pub fn add_var(v: ast::VarObj) {
    unsafe {
        VAR_VEC.insert(0, v);
    }
}

pub fn find_var(name: String) -> Option<ast::VarObj> {
    unsafe {
        for o in &VAR_VEC {
            if o.name == name {
                return Some(o.clone());
            }
        }
        None
    }
}

pub fn update_var_offset(name: String, offset: i64) {
    unsafe {
        for o in &mut VAR_VEC {
            if o.name == name {
                o.offset = offset;
            }
        }
    }
}

pub fn get_var_offset(name: String) -> i64 {
    unsafe {
        for o in &VAR_VEC {
            if o.name == name {
                return o.offset;
            }
        }
    }
    0
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

    /// unary = ("+" | "-" | "*" | "&") unary
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
            token::Tok::Ampersand => {
                use token::Tok::Ampersand;
                let pos = eat!(self, Ampersand);
                let expr = self.unary_expr()?;
                Ok(position::WithPos::new(ast::Expr::Addr(Box::new(expr)), pos))
            }
            token::Tok::Star => {
                use token::Tok::Star;
                let pos = eat!(self, Star);
                let expr = self.unary_expr()?;
                Ok(position::WithPos::new(
                    ast::Expr::Deref(Box::new(expr)),
                    pos,
                ))
            }
            _ => self.primary_expr(),
        }
    }

    /// funcall = ident "(" (assign ("," assign)*)? ")"
    fn funcall(&mut self, name: String) -> Result<ast::ExprWithPos> {
        use token::Tok::OpenParen;
        let pos = eat!(self, OpenParen);
        let mut args = vec![];
        while self.peek()?.token != token::Tok::CloseParen {
            if self.peek()?.token == token::Tok::Comma {
                use token::Tok::Comma;
                eat!(self, Comma);
            }
            args.push(self.assign_expr()?);
        }
        use token::Tok::CloseParen;
        eat!(self, CloseParen);
        Ok(ast::ExprWithPos {
            node: ast::Expr::FunCall {
                funcname: name,
                args,
            },
            pos,
        })
    }

    /// primary = "(" expr ")" | ident args? | num
    /// args = "(" ")"
    fn primary_expr(&mut self) -> Result<ast::ExprWithPos> {
        match self.peek()?.token {
            token::Tok::Number(_) => self.int_lit(),
            token::Tok::OpenParen => self.seq_exp(),
            token::Tok::Ident(_) => {
                let name;
                use token::Tok::Ident;
                let pos = eat!(self, Ident, name);
                // Function call
                if self.peek()?.token == token::Tok::OpenParen {
                    return self.funcall(name);
                }
                // Variable
                let var = find_var(name.clone());
                if var.is_none() {
                    panic!("undefined variable: {}", name);
                }
                Ok(ast::ExprWithPos::new(ast::Expr::Var(var.unwrap()), pos))
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

    /// expr-stmt = expr? ";"
    fn expr_stmt(&mut self) -> Result<ast::StmtWithPos<ast::ExprWithPos>> {
        match self.peek()?.token {
            token::Tok::Semicolon => {
                let pos = self.peek()?.pos;
                use token::Tok::Semicolon;
                eat!(self, Semicolon);
                let stmt = ast::StmtWithPos {
                    node: ast::Stmt::Null,
                    pos,
                };
                Ok(stmt)
            }
            _ => {
                let expr = self.expr()?;
                use token::Tok::Semicolon;
                eat!(self, Semicolon);
                let stmt = ast::StmtWithPos {
                    node: ast::Stmt::Expr(expr.clone()),
                    pos: expr.pos,
                };
                Ok(stmt)
            }
        }
    }

    /// compount-stmt = (declaration | stmt)* "}"
    fn compount_stmt(&mut self) -> Result<ast::StmtWithPos<ast::ExprWithPos>> {
        use token::Tok::OpenBrace;
        eat!(self, OpenBrace);
        let mut block = vec![];
        loop {
            if self.peek()?.token == token::Tok::CloseBrace {
                break;
            }
            match self.peek()?.token {
                token::Tok::KWInt => block.push(self.declaration()?),
                _ => block.push(self.stmt()?),
            }
        }
        use token::Tok::CloseBrace;
        let pos = eat!(self, CloseBrace);
        let stmt = ast::StmtWithPos {
            node: ast::Stmt::Block(block),
            pos: pos,
        };
        Ok(stmt)
    }

    /// stmt = "return" expr ";"
    ///      | "if" "(" expr ")" stmt ("else" stmt)?
    ///      | "for" "(" expr-stmt expr? ";" expr? ")" stmt
    ///      | "while" "(" expr ")" stmt
    ///      | "{" compount-stmt
    ///      | expr-stmt
    fn stmt(&mut self) -> Result<ast::StmtWithPos<ast::ExprWithPos>> {
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
            token::Tok::OpenBrace => self.compount_stmt(),
            token::Tok::If => {
                use token::Tok::If;
                let pos = eat!(self, If);
                use token::Tok::OpenParen;
                eat!(self, OpenParen);
                let cond = self.expr()?;
                use token::Tok::CloseParen;
                eat!(self, CloseParen);
                let then = self.stmt()?;
                let els;
                if self.peek()?.token == token::Tok::Else {
                    use token::Tok::Else;
                    eat!(self, Else);
                    els = Some(self.stmt()?);
                } else {
                    els = None;
                }
                let stmt = ast::StmtWithPos {
                    node: ast::Stmt::If {
                        cond: Box::new(cond),
                        then: Box::new(then),
                        els: Box::new(els),
                    },
                    pos,
                };
                Ok(stmt)
            }
            token::Tok::For => {
                use token::Tok::For;
                let pos = eat!(self, For);
                use token::Tok::OpenParen;
                eat!(self, OpenParen);
                let init = self.expr_stmt()?;
                let cond = if self.peek()?.token != token::Tok::Semicolon {
                    Some(self.expr()?)
                } else {
                    None
                };
                use token::Tok::Semicolon;
                eat!(self, Semicolon);
                let inc = if self.peek()?.token != token::Tok::CloseParen {
                    Some(self.expr()?)
                } else {
                    None
                };
                use token::Tok::CloseParen;
                eat!(self, CloseParen);
                let then = self.stmt()?;
                let stmt = ast::StmtWithPos {
                    node: ast::Stmt::For {
                        cond: Box::new(cond),
                        then: Box::new(then),
                        init: Box::new(init),
                        inc: Box::new(inc),
                    },
                    pos,
                };
                Ok(stmt)
            }
            token::Tok::While => {
                use token::Tok::While;
                let pos = eat!(self, While);
                use token::Tok::OpenParen;
                eat!(self, OpenParen);
                let cond = self.expr()?;
                use token::Tok::CloseParen;
                eat!(self, CloseParen);
                let then = self.stmt()?;
                let stmt = ast::StmtWithPos {
                    node: ast::Stmt::While {
                        cond: Box::new(cond),
                        then: Box::new(then),
                    },
                    pos,
                };
                Ok(stmt)
            }
            _ => self.expr_stmt(),
        }
    }

    /// declspec = "int"
    fn declspec(&mut self) -> Result<types::Type> {
        use token::Tok::KWInt;
        eat!(self, KWInt);
        Ok(types::Type::IntType)
    }

    /// declarator = "*"* ident
    fn declarator(&mut self, ty: types::Type) -> Result<types::Type> {
        let mut ty = ty;
        loop {
            match self.peek()?.token {
                token::Tok::Star => {
                    use token::Tok::Star;
                    eat!(self, Star);
                    ty = types::pointer_to(ty);
                }
                _ => break,
            }
        }

        match self.peek()?.token {
            token::Tok::Ident(_) => {
                ty = match ty {
                    types::Type::IntType { .. } => types::Type::IntType,
                    types::Type::PointerType { base, .. } => types::Type::PointerType { base },
                };
            }
            _ => panic!("expected a variable name"),
        }

        Ok(ty)
    }

    /// declaration = declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
    fn declaration(&mut self) -> Result<ast::StmtWithPos<ast::ExprWithPos>> {
        let basety = self.declspec()?;
        let mut stmts = vec![];
        let mut pos;

        loop {
            if self.peek()?.token == token::Tok::Semicolon {
                pos = self.peek()?.pos;
                break;
            }
            if self.peek()?.token == token::Tok::Comma {
                use token::Tok::Comma;
                eat!(self, Comma);
            }
            let ty = self.declarator(basety.clone())?;
            let name;
            use token::Tok::Ident;
            pos = eat!(self, Ident, name);
            let var = ast::VarObj {
                name: name.clone(),
                ty: ty,
                offset: 0,
            };
            add_var(var.clone());
            if self.peek()?.token != token::Tok::Equal {
                continue;
            }
            use token::Tok::Equal;
            eat!(self, Equal);
            let lhs = ast::ExprWithPos {
                node: ast::Expr::Var(var),
                pos,
            };
            let rhs = self.assign_expr()?;
            let node = ast::ExprWithPos {
                node: ast::Expr::Assign {
                    lvalue: Box::new(lhs),
                    rvalue: Box::new(rhs),
                },
                pos,
            };
            let stmt = ast::StmtWithPos {
                node: ast::Stmt::Expr(node),
                pos,
            };
            stmts.push(stmt);
        }

        let node = ast::StmtWithPos {
            node: ast::Stmt::Block(stmts),
            pos,
        };
        use token::Tok::Semicolon;
        eat!(self, Semicolon);
        Ok(node)
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

    pub fn parse(&mut self) -> Result<ast::Function<ast::ExprWithPos>> {
        let body = self.stmt()?;
        match self.token() {
            Ok(token::Token {
                token: token::Tok::EndOfFile,
                ..
            })
            | Err(error::Error::Eof) => Ok(ast::Function {
                body: Box::new(body),
                stack_size: 0,
            }),
            _ => Err(self.unexpected_token("end of file")?),
        }
    }
}
