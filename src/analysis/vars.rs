use std::collections::HashMap;

use crate::{ast::{ExprVisitor, StmtVisitor}, LoxError, errors};

#[derive(Debug)]
pub (super) struct VariableAnalyzer {
    pub (super) scopes: Vec<HashMap<String, bool>>,
}

impl VariableAnalyzer {
    pub fn declare<S: AsRef<str>>(&mut self, name: S) {
        if !self.scopes.is_empty() {
            self.scopes.last_mut().unwrap().insert(name.as_ref().to_string(), false);
        }
    }

    pub fn initialize<S: AsRef<str>>(&mut self, name: S) {
        if !self.scopes.is_empty() {
            self.scopes.last_mut().unwrap().insert(name.as_ref().to_string(), true);
        }
    }
}

impl Default for VariableAnalyzer {
    fn default() -> Self {
        let mut globals = HashMap::new();
        globals.insert("clock".to_string(), true);
        globals.insert("assert".to_string(), true);

        Self {
            scopes: vec![globals],
        }
    }
}

impl ExprVisitor<Vec<LoxError>> for VariableAnalyzer {
    fn visit_assign(&mut self, ident: &crate::lexer::Token, value: &crate::ast::Expr) -> Vec<LoxError> {
        let mut errs = self.visit_expr(value);

        for scope in self.scopes.iter().rev() {
            if scope.contains_key(ident.lexeme()) {
                return errs;
            }
        }

        errs.push(errors::user(
            &format!("Variable '{}' is not defined during assignment at {}.", ident.lexeme(), ident.location()),
            "Make sure you have defined the variable before assigning a value to it, or add the `var` keyword here.",
        ));

        errs
    }

    fn visit_binary(&mut self, left: &crate::ast::Expr, _op: &crate::lexer::Token, right: &crate::ast::Expr) -> Vec<LoxError> {
        vec![
            self.visit_expr(left),
            self.visit_expr(right),
        ].into_iter().flatten().collect()
    }

    fn visit_call(&mut self, callee: &crate::ast::Expr, args: &[crate::ast::Expr], _close: &crate::lexer::Token) -> Vec<LoxError> {
        vec![
            self.visit_expr(callee),
            args.iter().flat_map(|arg| self.visit_expr(arg)).collect(),
        ].into_iter().flatten().collect()
    }

    fn visit_fun_expr(&mut self, _token: &crate::lexer::Token, params: &[crate::lexer::Token], body: &[crate::ast::Stmt]) -> Vec<LoxError> {
        self.scopes.push(HashMap::new());

        for param in params {
            self.declare(param.lexeme());
            self.initialize(param.lexeme());
        }

        let errs = self.visit_block(body);

        self.scopes.pop();
        errs
    }

    fn visit_grouping(&mut self, expr: &crate::ast::Expr) -> Vec<LoxError> {
        self.visit_expr(expr)
    }

    fn visit_literal(&mut self, _value: &crate::ast::Literal) -> Vec<LoxError> {
        Vec::new()
    }

    fn visit_logical(&mut self, left: &crate::ast::Expr, _op: &crate::lexer::Token, right: &crate::ast::Expr) -> Vec<LoxError> {
        vec![
            self.visit_expr(left),
            self.visit_expr(right)
        ].into_iter().flatten().collect()
    }

    fn visit_unary(&mut self, _op: &crate::lexer::Token, expr: &crate::ast::Expr) -> Vec<LoxError> {
        self.visit_expr(expr)
    }

    fn visit_var_ref(&mut self, name: &crate::lexer::Token) -> Vec<LoxError> {
        if self.scopes.last().and_then(|s| s.get(name.lexeme()).map(|v| !(*v))).unwrap_or_default() {
            return vec![errors::user(
                "Variable references itself during initialization.",
                "Make sure you are not masking a variable with the same name and try using a different name for this variable if you are.",
            )];
        }

        for scope in self.scopes.iter().rev() {
            if scope.contains_key(name.lexeme()) {
                return vec![];
            }
        }

        vec![errors::user(
            &format!("Variable or function '{}' is not defined, referenced at {}.", name.lexeme(), name.location()),
            "Make sure you have defined the variable or function before attempting to reference it here.",
        )]
    }
}

impl StmtVisitor<Vec<LoxError>> for VariableAnalyzer {
    fn visit_break(&mut self) -> Vec<LoxError> {
        Vec::new()
    }

    fn visit_block(&mut self, stmts: &[crate::ast::Stmt]) -> Vec<LoxError> {
        self.scopes.push(HashMap::new());
        
        let mut errs = Vec::new();
        for stmt in stmts {
            errs.append(&mut self.visit_stmt(stmt));
        }

        self.scopes.pop();

        errs
    }

    fn visit_expr_stmt(&mut self, expr: &crate::ast::Expr) -> Vec<LoxError> {
        self.visit_expr(expr)
    }

    fn visit_fun_def(&mut self, name: &crate::lexer::Token, params: &[crate::lexer::Token], body: &[crate::ast::Stmt]) -> Vec<LoxError> {
        self.declare(name.lexeme());
        self.initialize(name.lexeme());

        self.scopes.push(HashMap::new());

        for param in params {
            self.declare(param.lexeme());
            self.initialize(param.lexeme());
        }

        let errs = self.visit_block(body);

        self.scopes.pop();
        errs
    }

    fn visit_if(&mut self, expr: &crate::ast::Expr, then_branch: &crate::ast::Stmt, else_branch: Option<&crate::ast::Stmt>) -> Vec<LoxError> {
        vec![
            self.visit_expr(expr),
            self.visit_stmt(then_branch),
            else_branch.map(|b| self.visit_stmt(b)).unwrap_or_default(),
        ].into_iter().flatten().collect()
    }

    fn visit_print(&mut self, expr: &crate::ast::Expr) -> Vec<LoxError> {
        self.visit_expr(expr)
    }

    fn visit_return(&mut self, _token: &crate::lexer::Token, expr: Option<&crate::ast::Expr>) -> Vec<LoxError> {
        expr.map(|e| self.visit_expr(e)).unwrap_or_default()
    }

    fn visit_var_def(&mut self, name: &crate::lexer::Token, expr: &crate::ast::Expr) -> Vec<LoxError> {
        self.declare(name.lexeme());
        let errs = self.visit_expr(expr);
        self.initialize(name.lexeme());
        errs
    }

    fn visit_while(&mut self, expr: &crate::ast::Expr, body: &crate::ast::Stmt) -> Vec<LoxError> {
        vec![
            self.visit_expr(expr),
            self.visit_stmt(body),
        ].into_iter().flatten().collect()
    }
}