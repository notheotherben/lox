use std::collections::{HashMap, HashSet};

use crate::{ast::{ExprVisitor, StmtVisitor, Stmt, Expr, FunType}, LoxError, errors, lexer::Token};

use super::Analyzer;

#[derive(Debug, PartialEq, Copy, Clone)]
pub (super) enum ClassType {
    None,
    Class,
    Subclass,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub (super) enum FunctionType {
    None,
    Function,
    Initializer,
}

#[derive(Debug)]
pub (super) struct VariableAnalyzer {
    pub (super) scopes: Vec<HashMap<String, bool>>,
    pub (super) current_class: ClassType,
    pub (super) current_function: FunctionType,
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
            current_class: ClassType::None,
            current_function: FunctionType::None,
        }
    }
}

impl Analyzer for VariableAnalyzer {}

impl ExprVisitor<Vec<LoxError>> for VariableAnalyzer {
    fn visit_assign(&mut self, ident: &Token, value: &Expr) -> Vec<LoxError> {
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

    fn visit_binary(&mut self, left: &Expr, _op: &Token, right: &Expr) -> Vec<LoxError> {
        vec![
            self.visit_expr(left),
            self.visit_expr(right),
        ].into_iter().flatten().collect()
    }

    fn visit_call(&mut self, callee: &Expr, args: &[Expr], _close: &Token) -> Vec<LoxError> {
        vec![
            self.visit_expr(callee),
            args.iter().flat_map(|arg| self.visit_expr(arg)).collect(),
        ].into_iter().flatten().collect()
    }

    fn visit_get(&mut self, object: &Expr, _property: &Token) -> Vec<LoxError> {
        vec![
            self.visit_expr(object),
        ].into_iter().flatten().collect()
    }

    fn visit_fun_expr(&mut self, _token: &Token, params: &[Token], body: &[Stmt]) -> Vec<LoxError> {
        self.scopes.push(HashMap::new());

        let mut errs = Vec::new();
        let mut known_params = HashSet::new();
        for param in params {
            self.declare(param.lexeme());
            self.initialize(param.lexeme());
            if known_params.contains(param.lexeme()) {
                errs.push(errors::user(
                    &format!("Duplicate parameter '{}' at {}.", param.lexeme(), param.location()),
                    "Make sure you are not using the same parameter name twice.",
                ));
            } else {
                known_params.insert(param.lexeme());
            }
        }

        for stmt in body {
            errs.append(&mut self.visit_stmt(stmt));
        }

        self.scopes.pop();
        errs
    }

    fn visit_grouping(&mut self, expr: &Expr) -> Vec<LoxError> {
        self.visit_expr(expr)
    }

    fn visit_literal(&mut self, _value: &crate::ast::Literal) -> Vec<LoxError> {
        Vec::new()
    }

    fn visit_logical(&mut self, left: &Expr, _op: &Token, right: &Expr) -> Vec<LoxError> {
        vec![
            self.visit_expr(left),
            self.visit_expr(right)
        ].into_iter().flatten().collect()
    }

    fn visit_set(&mut self, object: &Expr, _property: &Token, value: &Expr) -> Vec<LoxError> {
        vec![
            self.visit_expr(object),
            self.visit_expr(value),
        ].into_iter().flatten().collect()
    }

    fn visit_super(&mut self, keyword: &Token, _method: &Token) -> Vec<LoxError> {
        match self.current_class {
            ClassType::None => vec![errors::user(
                &format!("Cannot use 'super' outside of a class at {}.", keyword.location()),
                "Make sure you are inside a class, and that you are using 'super' in the correct context.",
            )],
            ClassType::Class => vec![errors::user(
                &format!("Cannot use 'super' in a class with no superclass at {}.", keyword.location()),
                "Make sure you are inside a class, and that you are using 'super' in the correct context.",
            )],
            ClassType::Subclass => Vec::new(),
        }
    }

    fn visit_this(&mut self, keyword: &Token) -> Vec<LoxError> {
        for scope in self.scopes.iter().rev() {
            if scope.contains_key("this") {
                return vec![];
            }
        }

        vec![errors::user(
            &format!("Found a usage of '{}' outside a class method at {}.", keyword.lexeme(), keyword.location()),
            "You can only access `this` within a class method.",
        )]
    }

    fn visit_unary(&mut self, _op: &Token, expr: &Expr) -> Vec<LoxError> {
        self.visit_expr(expr)
    }

    fn visit_var_ref(&mut self, name: &Token) -> Vec<LoxError> {
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

    fn visit_block(&mut self, stmts: &[Stmt]) -> Vec<LoxError> {
        self.scopes.push(HashMap::new());
        
        let mut errs = Vec::new();
        for stmt in stmts {
            errs.append(&mut self.visit_stmt(stmt));
        }

        self.scopes.pop();

        errs
    }

    fn visit_class(&mut self, name: &Token, superclass: Option<&Expr>, statics: &[Stmt], methods: &[Stmt]) -> Vec<LoxError> {
        let errs = if let Some(superclass) = superclass {
            self.visit_expr(superclass)
        } else {
            Vec::new()
        };

        self.declare(name.lexeme());
        self.initialize(name.lexeme());
        let parent_class = self.current_class;
        self.current_class = if superclass.is_some() { ClassType::Subclass } else { ClassType::Class };

        let errs: Vec<LoxError> = vec![
            errs,
            statics.iter().flat_map(|stmt| self.visit_stmt(stmt)).collect()
        ].into_iter().flatten().collect();

        self.scopes.push(HashMap::new());
        self.declare("this");
        self.initialize("this");

        if superclass.is_some() {
            self.declare("super");
            self.initialize("super");
        }

        let errs = vec![
            errs,
            methods.iter().flat_map(|stmt| self.visit_stmt(stmt)).collect()
        ].into_iter().flatten().collect();

        self.scopes.pop();
        self.current_class = parent_class;

        errs
    }

    fn visit_expr_stmt(&mut self, expr: &Expr) -> Vec<LoxError> {
        self.visit_expr(expr)
    }

    fn visit_fun_def(&mut self, ty: FunType, name: &Token, params: &[Token], body: &[Stmt]) -> Vec<LoxError> {
        self.declare(name.lexeme());
        self.initialize(name.lexeme());
        
        let parent_fun = self.current_function;
        self.current_function = if ty == FunType::Initializer { FunctionType::Initializer } else { FunctionType::Function };

        self.scopes.push(HashMap::new());

        let mut errs = Vec::new();
        
        let mut known_params = HashSet::new();
        for param in params {
            self.declare(param.lexeme());
            self.initialize(param.lexeme());
            if known_params.contains(param.lexeme()) {
                errs.push(errors::user(
                    &format!("Duplicate parameter '{}' at {}.", param.lexeme(), param.location()),
                    "Make sure you are not using the same parameter name twice.",
                ));
            } else {
                known_params.insert(param.lexeme());
            }
        }

        for stmt in body {
            errs.append(&mut self.visit_stmt(stmt));
        }

        self.scopes.pop();
        self.current_function = parent_fun;

        errs
    }

    fn visit_if(&mut self, expr: &Expr, then_branch: &Stmt, else_branch: Option<&Stmt>) -> Vec<LoxError> {
        vec![
            self.visit_expr(expr),
            self.visit_stmt(then_branch),
            else_branch.map(|b| self.visit_stmt(b)).unwrap_or_default(),
        ].into_iter().flatten().collect()
    }

    fn visit_print(&mut self, expr: &Expr) -> Vec<LoxError> {
        self.visit_expr(expr)
    }

    fn visit_return(&mut self, token: &Token, expr: Option<&Expr>) -> Vec<LoxError> {
        match self.current_function {
            FunctionType::Initializer if expr.is_some() => {
                vec![errors::user(
                    &format!("Found an attempt to return a value within a class initializer at {}.", token.location()),
                    "You cannot return a value within a class initializer.",
                )]
            },
            FunctionType::None => return vec![errors::user(
                &format!("Return statement used outside of a function at {}.", token.location()),
                "You can only use the `return` keyword within a function.",
            )],
            _ => expr.map(|e| self.visit_expr(e)).unwrap_or_default()
        }
    }

    fn visit_var_def(&mut self, name: &Token, expr: &Expr) -> Vec<LoxError> {
        let errs = if self.scopes.last().map(|s| s.contains_key(name.lexeme())).unwrap_or_default() {
            vec![errors::user(
                &format!("Variable '{}' is already defined in this scope, duplicate declaration found at {}.", name.lexeme(), name.location()),
                "Remove the `var` keyword to assign a new value to this variable, or rename it if you intended to maintain a separate instance.",
            )]
        } else {
            Vec::new()
        };

        self.declare(name.lexeme());
        let errs = vec![
            errs,
            self.visit_expr(expr)
        ].into_iter().flatten().collect();
        self.initialize(name.lexeme());
        errs
    }

    fn visit_while(&mut self, expr: &Expr, body: &Stmt) -> Vec<LoxError> {
        vec![
            self.visit_expr(expr),
            self.visit_stmt(body),
        ].into_iter().flatten().collect()
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::Scanner, ast::Parser, analysis::Analyzer};

    #[test]
    fn duplicate_local_variables() {
        let mut analyzer = super::VariableAnalyzer::default();

        let (tree, errs) = Parser::parse(
            &mut Scanner::new(
                r#"
                var a = 1;
                var a = 2;
                "#
            ).filter_map(|t| t.ok())
        );

        assert!(errs.is_empty(), "no parsing errors");

        let errs = analyzer.analyze(&tree);
        assert_eq!(errs.len(), 1, "expected 1 error");
        assert_eq!(errs[0].description(), "Variable 'a' is already defined in this scope, duplicate declaration found at line 3, column 21.");
    }

    #[test]
    fn this_outside_class_method() {
        let mut analyzer = super::VariableAnalyzer::default();

        let (tree, errs) = Parser::parse(
            &mut Scanner::new(
                r#"
                var a = 1;
                this.a = 2;
                "#
            ).filter_map(|t| t.ok())
        );

        assert!(errs.is_empty(), "no parsing errors");

        let errs = analyzer.analyze(&tree);
        assert_eq!(errs.len(), 1, "expected 1 error");
        assert_eq!(errs[0].description(), "Found a usage of 'this' outside a class method at line 3, column 17.");
    }

    #[test]
    fn return_inside_constructor() {
        let mut analyzer = super::VariableAnalyzer::default();

        let (tree, errs) = Parser::parse(
            &mut Scanner::new(
                r#"
                class A {
                    init() {
                        return 1;
                    }
                }
                "#
            ).filter_map(|t| t.ok())
        );

        assert!(errs.is_empty(), "no parsing errors");

        let errs = analyzer.analyze(&tree);
        assert_eq!(errs.len(), 1, "expected 1 error");
        assert_eq!(errs[0].description(), "Found an attempt to return a value within a class initializer at line 4, column 25.");
    }

    #[test]
    fn super_outside_class_method() {
        let mut analyzer = super::VariableAnalyzer::default();

        let (tree, errs) = Parser::parse(
            &mut Scanner::new(
                r#"
                super.a();
                "#
            ).filter_map(|t| t.ok())
        );

        assert!(errs.is_empty(), "no parsing errors");

        let errs = analyzer.analyze(&tree);
        assert_eq!(errs.len(), 1, "expected 1 error");
        assert_eq!(errs[0].description(), "Cannot use 'super' outside of a class at line 2, column 17.");
    }

    #[test]
    fn super_in_root_class() {
        let mut analyzer = super::VariableAnalyzer::default();

        let (tree, errs) = Parser::parse(
            &mut Scanner::new(
                r#"
                class A {
                    init() {
                        super.a();
                    }
                }
                "#
            ).filter_map(|t| t.ok())
        );

        assert!(errs.is_empty(), "no parsing errors");

        let errs = analyzer.analyze(&tree);
        assert_eq!(errs.len(), 1, "expected 1 error");
        assert_eq!(errs[0].description(), "Cannot use 'super' in a class with no superclass at line 4, column 25.");
    }
}