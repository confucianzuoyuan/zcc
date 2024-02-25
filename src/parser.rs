use crate::{ast, error, lexer, position, symbol, token};

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
                ast::Expr::Oper {
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
        let mut expr = self.primary_expr()?;
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
            let right = Box::new(self.primary_expr()?);
            let pos = expr.pos.grow(right.pos);
            expr = position::WithPos::new(
                ast::Expr::Oper {
                    left: Box::new(expr),
                    oper,
                    right,
                },
                pos,
            );
        }
        Ok(expr)
    }

    // primary = "(" expr ")" | num
    fn primary_expr(&mut self) -> Result<ast::ExprWithPos> {
        match self.peek()?.token {
            token::Tok::Number(_) => self.int_lit(),
            token::Tok::OpenParen => self.seq_exp(),
            _ => Err(self.unexpected_token("integer literal, (")?),
        }
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
        self.additive_expr()
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

    pub fn parse(&mut self) -> Result<ast::ExprWithPos> {
        let main_expression = self.expr()?;
        match self.token() {
            Ok(token::Token {
                token: token::Tok::EndOfFile,
                ..
            })
            | Err(error::Error::Eof) => Ok(main_expression),
            _ => Err(self.unexpected_token("end of file")?),
        }
    }
}
