use crate::{ast, lexer, token, types};

#[derive(Debug, Clone, PartialEq)]
pub enum Declarator {
    Ident(String),
    PointerDeclarator(Box<Declarator>),
    FunDeclarator(Vec<ParamInfo>, Box<Declarator>),
    ArrayDeclarator(Box<Declarator>, usize),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParamInfo {
    Param(types::Type, Declarator),
}

fn get_precedence(t: token::Token) -> Option<i64> {
    match t {
        token::Token::Star | token::Token::Slash => Some(50),
        token::Token::Plus | token::Token::Minus => Some(45),
        token::Token::LesserThan
        | token::Token::LesserOrEqual
        | token::Token::GreaterOrEqual
        | token::Token::GreaterThan => Some(35),
        token::Token::DoubleEqual | token::Token::BangEqual => Some(30),
        token::Token::Equal => Some(1),
        _ => None,
    }
}

pub struct Parser<R: std::io::Read> {
    lexer: lexer::Lexer<R>,
    lookahead: Option<token::Token>,
}

impl<R: std::io::Read> Parser<R> {
    pub fn new(lexer: lexer::Lexer<R>) -> Self {
        Parser {
            lexer,
            lookahead: None,
        }
    }

    fn peek_token(&mut self) -> token::Token {
        if self.lookahead.is_none() {
            self.lookahead = Some(self.lexer.token());
        }
        self.lookahead.as_ref().unwrap().clone()
    }

    fn token(&mut self) -> token::Token {
        if let Some(token) = self.lookahead.take() {
            return token;
        }
        self.lexer.token()
    }

    fn eat(&mut self, expected: token::Token) {
        let token = self.token();
        if token != expected {
            panic!("expected token `{}`, but got token `{}`", expected, token);
        }
    }

    fn parse_exp_loop(
        &mut self,
        min_prec: i64,
        left: ast::UntypedExp,
        next: token::Token,
    ) -> ast::UntypedExp {
        match get_precedence(next.clone()) {
            Some(prec) if prec >= min_prec => match next {
                token::Token::Equal => {
                    self.eat(token::Token::Equal);
                    let right = self.parse_expression(prec);
                    let left = ast::UntypedExp::Assignment(Box::new(left), Box::new(right));
                    let next_token = self.peek_token();
                    self.parse_exp_loop(min_prec, left, next_token)
                }
                _ => {
                    let operator = self.parse_binop();
                    let right = self.parse_expression(prec + 1);
                    let left = ast::UntypedExp::Binary(operator, Box::new(left), Box::new(right));
                    let next_token = self.peek_token();
                    self.parse_exp_loop(min_prec, left, next_token)
                }
            },
            _ => left,
        }
    }

    fn parse_binop(&mut self) -> ast::BinaryOperator {
        let next_token = self.peek_token();
        self.eat(next_token.clone());
        match next_token {
            token::Token::Plus => ast::BinaryOperator::Add,
            token::Token::Minus => ast::BinaryOperator::Subtract,
            token::Token::Star => ast::BinaryOperator::Multiply,
            token::Token::Slash => ast::BinaryOperator::Divide,
            token::Token::DoubleEqual => ast::BinaryOperator::Equal,
            token::Token::BangEqual => ast::BinaryOperator::NotEqual,
            token::Token::LesserThan => ast::BinaryOperator::LessThan,
            token::Token::LesserOrEqual => ast::BinaryOperator::LessOrEqual,
            token::Token::GreaterThan => ast::BinaryOperator::GreaterThan,
            token::Token::GreaterOrEqual => ast::BinaryOperator::GreaterOrEqual,
            _ => panic!("Internal error when parsing binary operator"),
        }
    }

    fn parse_unop(&mut self) -> ast::UnaryOperator {
        match self.peek_token() {
            token::Token::Minus => {
                self.eat(token::Token::Minus);
                ast::UnaryOperator::Negate
            }
            _ => panic!("Internal error when parsing unary operator"),
        }
    }

    fn parse_unary_expression(&mut self) -> ast::UntypedExp {
        match self.peek_token() {
            token::Token::Star => {
                self.eat(token::Token::Star);
                let inner_exp = self.parse_unary_expression();
                ast::UntypedExp::Dereference(Box::new(inner_exp))
            }
            token::Token::Ampersand => {
                self.eat(token::Token::Ampersand);
                let inner_exp = self.parse_unary_expression();
                ast::UntypedExp::AddrOf(Box::new(inner_exp))
            }
            token::Token::Minus => {
                let operator = self.parse_unop();
                let inner_exp = self.parse_unary_expression();
                ast::UntypedExp::Unary(operator, Box::new(inner_exp))
            }
            token::Token::Plus => {
                self.eat(token::Token::Plus);
                self.parse_unary_expression()
            }
            _ => self.parse_postfix_expression(),
        }
    }

    fn parse_postfix_expression(&mut self) -> ast::UntypedExp {
        let mut node = self.parse_primary_expression();

        while self.peek_token() == token::Token::OpenBracket {
            // x[y] is short for *(x+y)
            self.eat(token::Token::OpenBracket);
            let idx = self.parse_expression(0);
            self.eat(token::Token::CloseBracket);
            node = ast::UntypedExp::Dereference(Box::new(ast::UntypedExp::Binary(
                ast::BinaryOperator::Add,
                Box::new(node),
                Box::new(idx),
            )));
        }

        node
    }

    fn parse_primary_expression(&mut self) -> ast::UntypedExp {
        match self.peek_token() {
            token::Token::Number(i) => {
                self.eat(token::Token::Number(i));
                ast::UntypedExp::Constant(i)
            }
            token::Token::Ident(_) => {
                let id = self.parse_id();
                match self.peek_token() {
                    token::Token::OpenParen => {
                        let args = self.parse_optional_arg_list();
                        ast::UntypedExp::FunCall { f: id, args }
                    }
                    _ => ast::UntypedExp::Var(id),
                }
            }
            token::Token::OpenParen => {
                self.eat(token::Token::OpenParen);
                let e = self.parse_expression(0);
                self.eat(token::Token::CloseParen);
                e
            }
            token::Token::KWSizeOf => {
                self.eat(token::Token::KWSizeOf);
                let node = self.parse_unary_expression();
                ast::UntypedExp::SizeOf(Box::new(node))
            }
            token::Token::String(string) => {
                self.eat(token::Token::String(string.clone()));
                ast::UntypedExp::String(string)
            }
            other => panic!("expected: a primary expression, actual: {}", other),
        }
    }

    fn parse_optional_arg_list(&mut self) -> Vec<ast::UntypedExp> {
        self.eat(token::Token::OpenParen);
        let args = match self.peek_token() {
            token::Token::CloseParen => vec![],
            _ => self.parse_arg_list(),
        };
        self.eat(token::Token::CloseParen);
        args
    }

    fn parse_arg_list(&mut self) -> Vec<ast::UntypedExp> {
        let arg = self.parse_expression(0);
        match self.peek_token() {
            token::Token::Comma => {
                self.eat(token::Token::Comma);
                let mut arg_list = vec![];
                arg_list.push(arg);
                arg_list.append(&mut self.parse_arg_list());
                arg_list
            }
            _ => vec![arg],
        }
    }

    fn parse_id(&mut self) -> String {
        match self.peek_token() {
            token::Token::Ident(x) => {
                self.eat(token::Token::Ident(x.clone()));
                x
            }
            other => panic!("expected: an identifier, actual: {}", other),
        }
    }

    fn parse_expression(&mut self, min_prec: i64) -> ast::UntypedExp {
        let initial_factor = self.parse_unary_expression();
        let next_token = self.peek_token();
        self.parse_exp_loop(min_prec, initial_factor, next_token)
    }

    fn is_specifier(&mut self, token: token::Token) -> bool {
        match token {
            token::Token::KWInt | token::Token::KWChar => true,
            _ => false,
        }
    }

    fn parse_simple_declarator(&mut self) -> Declarator {
        match self.peek_token() {
            token::Token::OpenParen => {
                self.eat(token::Token::OpenParen);
                let decl = self.parse_declarator();
                self.eat(token::Token::CloseParen);
                decl
            }
            token::Token::Ident(id) => {
                self.eat(token::Token::Ident(id.clone()));
                Declarator::Ident(id)
            }
            other => panic!("expected: a simple declarator, actual: {}", other),
        }
    }

    fn parse_direct_declarator(&mut self) -> Declarator {
        let simple_dec = self.parse_simple_declarator();
        match self.peek_token() {
            token::Token::OpenParen => {
                let params = self.parse_param_list();
                Declarator::FunDeclarator(params, Box::new(simple_dec))
            }
            token::Token::OpenBracket => {
                let array_dimensions = self.parse_array_dimensions();
                let mut array_dec = simple_dec.clone();
                for dim in array_dimensions {
                    array_dec = Declarator::ArrayDeclarator(Box::new(array_dec), dim);
                }
                array_dec
            }
            _ => simple_dec,
        }
    }

    fn parse_param_list(&mut self) -> Vec<ParamInfo> {
        self.eat(token::Token::OpenParen);
        if self.peek_token() != token::Token::CloseParen {
            let params = self.param_loop();
            self.eat(token::Token::CloseParen);
            params
        } else {
            self.eat(token::Token::CloseParen);
            vec![]
        }
    }

    fn param_loop(&mut self) -> Vec<ParamInfo> {
        let p = self.parse_param();
        match self.peek_token() {
            token::Token::Comma => {
                let mut ps = vec![];
                self.eat(token::Token::Comma);
                ps.push(p);
                ps.append(&mut self.param_loop());
                ps
            }
            _ => vec![p],
        }
    }

    fn parse_type(&mut self) -> types::Type {
        match self.peek_token() {
            token::Token::KWInt => {
                self.eat(token::Token::KWInt);
                types::Type::Int
            }
            token::Token::KWChar => {
                self.eat(token::Token::KWChar);
                types::Type::Char
            }
            _ => panic!("not valid type specifier"),
        }
    }

    fn parse_param(&mut self) -> ParamInfo {
        let param_type = self.parse_type();
        let param_decl = self.parse_declarator();
        ParamInfo::Param(param_type, param_decl)
    }

    fn parse_declarator(&mut self) -> Declarator {
        match self.peek_token() {
            token::Token::Star => {
                self.eat(token::Token::Star);
                let inner = self.parse_declarator();
                Declarator::PointerDeclarator(Box::new(inner))
            }
            _ => self.parse_direct_declarator(),
        }
    }

    fn parse_variable_declaration(&mut self) -> ast::VariableDeclarations<ast::UntypedInitializer> {
        match self.parse_declaration() {
            ast::Declaration::VarDecl(vd) => vd,
            ast::Declaration::FunDecl(_) => {
                panic!("Expected variable declaration but found function declaration")
            }
        }
    }

    fn process_param(&mut self, param: ParamInfo) -> (String, types::Type) {
        match param {
            ParamInfo::Param(p_base_type, p_decl) => {
                let (param_name, param_t, _) = self.process_declarator(p_decl, p_base_type);
                match param_t {
                    types::Type::FunType { .. } => {
                        panic!("Function pointers in parameters are not supported")
                    }
                    _ => (),
                };
                (param_name, param_t)
            }
        }
    }

    fn process_declarator(
        &mut self,
        decl: Declarator,
        base_type: types::Type,
    ) -> (String, types::Type, Vec<String>) {
        match decl {
            Declarator::Ident(s) => (s, base_type, vec![]),
            Declarator::PointerDeclarator(d) => {
                let derived_type = types::Type::Pointer(Box::new(base_type));
                self.process_declarator(*d, derived_type)
            }
            Declarator::ArrayDeclarator(inner, cnst) => {
                let size = cnst;
                let derived_type = types::Type::Array {
                    elem_type: Box::new(base_type),
                    size,
                };
                self.process_declarator(*inner, derived_type)
            }
            Declarator::FunDeclarator(params, decl) => match *decl {
                Declarator::Ident(s) => {
                    let mut param_names = vec![];
                    let mut param_types = vec![];
                    for param in params {
                        let t = self.process_param(param);
                        param_names.push(t.0);
                        param_types.push(t.1);
                    }
                    let fun_type = types::Type::FunType {
                        param_types,
                        ret_type: Box::new(base_type),
                    };
                    (s, fun_type, param_names)
                }
                _ => panic!("cannot apply additional type derivations to a function declarator"),
            },
        }
    }

    fn parse_statement(&mut self) -> ast::Statement<ast::UntypedInitializer, ast::UntypedExp> {
        match self.peek_token() {
            token::Token::KWIf => self.parse_if_statement(),
            token::Token::OpenBrace => ast::Statement::Compound(self.parse_block()),
            token::Token::KWWhile => self.parse_while_loop(),
            token::Token::KWFor => self.parse_for_loop(),
            token::Token::KWReturn => {
                self.eat(token::Token::KWReturn);
                let exp = self.parse_optional_expression(token::Token::Semicolon);
                ast::Statement::Return(exp)
            }
            _ => {
                let opt_exp = self.parse_optional_expression(token::Token::Semicolon);
                match opt_exp {
                    Some(exp) => ast::Statement::Expression(exp),
                    None => ast::Statement::Null,
                }
            }
        }
    }

    fn parse_if_statement(&mut self) -> ast::Statement<ast::UntypedInitializer, ast::UntypedExp> {
        self.eat(token::Token::KWIf);
        self.eat(token::Token::OpenParen);
        let condition = self.parse_expression(0);
        self.eat(token::Token::CloseParen);
        let then_clause = self.parse_statement();
        let else_clause = match self.peek_token() {
            token::Token::KWElse => {
                self.eat(token::Token::KWElse);
                Some(self.parse_statement())
            }
            _ => None,
        };
        ast::Statement::If {
            condition,
            then_clause: Box::new(then_clause),
            else_clause: Box::new(else_clause),
        }
    }

    fn parse_while_loop(&mut self) -> ast::Statement<ast::UntypedInitializer, ast::UntypedExp> {
        self.eat(token::Token::KWWhile);
        self.eat(token::Token::OpenParen);
        let condition = self.parse_expression(0);
        self.eat(token::Token::CloseParen);
        let body = self.parse_statement();
        ast::Statement::While {
            condition,
            body: Box::new(body),
            id: "".to_string(),
        }
    }

    fn parse_for_loop(&mut self) -> ast::Statement<ast::UntypedInitializer, ast::UntypedExp> {
        self.eat(token::Token::KWFor);
        self.eat(token::Token::OpenParen);
        let init = self.parse_for_init();
        let condition = self.parse_optional_expression(token::Token::Semicolon);
        let post = self.parse_optional_expression(token::Token::CloseParen);
        let body = self.parse_statement();
        ast::Statement::For {
            init,
            condition,
            post,
            body: Box::new(body),
            id: "".to_string(),
        }
    }

    fn parse_optional_expression(&mut self, delim: token::Token) -> Option<ast::UntypedExp> {
        if self.peek_token() == delim {
            self.eat(delim);
            None
        } else {
            let e = self.parse_expression(0);
            self.eat(delim);
            Some(e)
        }
    }

    fn parse_block_item(&mut self) -> ast::BlockItem<ast::UntypedInitializer, ast::UntypedExp> {
        let current_token = self.peek_token();
        if self.is_specifier(current_token) {
            ast::BlockItem::D(self.parse_declaration())
        } else {
            ast::BlockItem::S(self.parse_statement())
        }
    }

    fn parse_block_item_list(
        &mut self,
    ) -> Vec<ast::BlockItem<ast::UntypedInitializer, ast::UntypedExp>> {
        match self.peek_token() {
            token::Token::CloseBrace => vec![],
            _ => {
                let mut block_item_list = vec![];
                let next_block_item = self.parse_block_item();
                block_item_list.push(next_block_item);
                block_item_list.append(&mut self.parse_block_item_list());
                block_item_list
            }
        }
    }

    fn parse_block(&mut self) -> ast::Block<ast::UntypedInitializer, ast::UntypedExp> {
        self.eat(token::Token::OpenBrace);
        let block_items = self.parse_block_item_list();
        self.eat(token::Token::CloseBrace);
        block_items
    }

    fn finish_parsing_function_declaration(
        &mut self,
        fun_type: types::Type,
        name: String,
        params: Vec<String>,
    ) -> ast::FunctionDeclaration<ast::UntypedInitializer, ast::UntypedExp> {
        let body = match self.peek_token() {
            token::Token::OpenBrace => Some(self.parse_block()),
            token::Token::Semicolon => {
                self.eat(token::Token::Semicolon);
                None
            }
            other => panic!("expected function body or semicolon, actual: {}", other),
        };
        ast::FunctionDeclaration {
            name,
            fun_type,
            params,
            body,
        }
    }

    fn parsing_variable_declaration_helper(
        &mut self,
        t: types::Type,
    ) -> ast::VariableDeclaration<ast::UntypedInitializer> {
        match self.peek_token() {
            token::Token::Ident(id) => {
                self.eat(token::Token::Ident(id.clone()));
                if self.peek_token() == token::Token::Equal {
                    self.eat(token::Token::Equal);
                    let init = self.parse_initializer();
                    ast::VariableDeclaration {
                        name: id,
                        var_type: t,
                        init: Some(init),
                    }
                } else {
                    ast::VariableDeclaration {
                        name: id,
                        var_type: t,
                        init: None,
                    }
                }
            }
            other => panic!(
                "expected: An initializer or semicolon or comma, actual: {}",
                other
            ),
        }
    }

    fn finish_parsing_variable_declaration(
        &mut self,
        var_type: types::Type,
        name: String,
    ) -> ast::VariableDeclarations<ast::UntypedInitializer> {
        match self.peek_token() {
            // 处理 `int i;`这种情况
            token::Token::Semicolon => {
                self.eat(token::Token::Semicolon);
                vec![ast::VariableDeclaration {
                    name,
                    var_type,
                    init: None,
                }]
            }
            token::Token::Comma => {
                let mut var_decls = vec![ast::VariableDeclaration {
                    name,
                    var_type: var_type.clone(),
                    init: None,
                }];
                while self.peek_token() == token::Token::Comma {
                    self.eat(token::Token::Comma);
                    var_decls.push(self.parsing_variable_declaration_helper(var_type.clone()));
                }
                self.eat(token::Token::Semicolon);
                var_decls
            }
            token::Token::Equal => {
                self.eat(token::Token::Equal);
                let init = self.parse_initializer();
                let mut var_decls = vec![ast::VariableDeclaration {
                    name,
                    var_type: var_type.clone(),
                    init: Some(init),
                }];
                while self.peek_token() == token::Token::Comma {
                    self.eat(token::Token::Comma);
                    var_decls.push(self.parsing_variable_declaration_helper(var_type.clone()));
                }
                self.eat(token::Token::Semicolon);
                var_decls
            }
            other => panic!("expected: An initializer or semicolon, actual: {}", other),
        }
    }

    fn parse_initializer(&mut self) -> ast::UntypedInitializer {
        if self.peek_token() == token::Token::OpenBrace {
            self.eat(token::Token::OpenBrace);
            let init_list = self.parse_init_list();
            self.eat(token::Token::CloseBrace);
            ast::UntypedInitializer::CompoundInit(init_list)
        } else {
            let e = self.parse_expression(0);
            ast::UntypedInitializer::SingleInit(e)
        }
    }

    fn parse_init_list(&mut self) -> Vec<ast::UntypedInitializer> {
        let next_init = self.parse_initializer();
        match self.peek_token() {
            token::Token::Comma => {
                self.eat(token::Token::Comma);
                if self.peek_token() == token::Token::CloseBrace {
                    vec![next_init]
                } else {
                    let mut init_list = vec![];
                    init_list.push(next_init);
                    init_list.append(&mut self.parse_init_list());
                    init_list
                }
            }
            _ => vec![next_init],
        }
    }

    fn parse_declaration(&mut self) -> ast::Declaration<ast::UntypedInitializer, ast::UntypedExp> {
        let base_typ = self.parse_type();
        let declarator = self.parse_declarator();
        let (name, typ, params) = self.process_declarator(declarator, base_typ);
        match typ {
            types::Type::FunType { .. } => ast::Declaration::FunDecl(
                self.finish_parsing_function_declaration(typ, name, params),
            ),
            _ => {
                if params.len() == 0 {
                    ast::Declaration::VarDecl(self.finish_parsing_variable_declaration(typ, name))
                } else {
                    panic!("Internal error: should not reach here")
                }
            }
        }
    }

    fn parse_constant(&mut self) -> usize {
        match self.peek_token() {
            token::Token::Number(i) => {
                self.eat(token::Token::Number(i));
                i as usize
            }
            _ => panic!(),
        }
    }

    /// { "[" <const> "]" }+
    fn parse_array_dimensions(&mut self) -> Vec<usize> {
        match self.peek_token() {
            token::Token::OpenBracket => {
                let mut dims = vec![];
                self.eat(token::Token::OpenBracket);
                let dim = self.parse_constant();
                self.eat(token::Token::CloseBracket);
                dims.push(dim);
                dims.append(&mut self.parse_array_dimensions());
                dims
            }
            _ => vec![],
        }
    }

    /// <for-init> ::= <declaration> | [ <exp> ] ";"
    fn parse_for_init(&mut self) -> ast::ForInit<ast::UntypedInitializer, ast::UntypedExp> {
        let next_token = self.peek_token();
        if self.is_specifier(next_token) {
            ast::ForInit::InitDecl(self.parse_variable_declaration())
        } else {
            let opt_e = self.parse_optional_expression(token::Token::Semicolon);
            ast::ForInit::InitExp(opt_e)
        }
    }

    /// { <function-declaration> }
    fn parse_declaration_list(
        &mut self,
    ) -> Vec<ast::Declaration<ast::UntypedInitializer, ast::UntypedExp>> {
        match self.peek_token() {
            token::Token::EndOfFile => vec![],
            _ => {
                let next_decl = self.parse_declaration();
                let mut decls = vec![];
                decls.push(next_decl);
                decls.append(&mut self.parse_declaration_list());
                decls
            }
        }
    }

    pub fn parse(&mut self) -> ast::UntypedProgram {
        self.parse_declaration_list()
    }
}
