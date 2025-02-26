use super::{
    attr::{Attribute, Attributes},
    to_code::{ConstructPrettyCode, ConstructWgslCode, TAB_SIZE},
    util,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WgslFn {
    /// Attributes of the function.
    ///
    /// e.g. **@fragment** fn fragmentMain(...) {...}
    pub attrs: Attributes,

    /// Name of the function.
    ///
    /// e.g. fn **foo**(...) {...}
    pub ident: String,

    /// Inputs of the function.
    ///
    /// e.g. fn foo(**input : Input**) {...}
    pub inputs: Vec<FnParam>,

    /// Output of the function.
    ///
    /// e.g. fn foo(...) -> **Output** {...}
    pub output: Option<FnParam>,

    /// Statements of the function.
    ///
    /// e.g. fn foo(...) { **statement; { statement } ...** }
    pub stmt: CompoundStatement,
}

impl WgslFn {
    /// Tries to remove the statement.
    pub fn remove_statement(
        &mut self,
        attr_outer: &str,
        attr_inner: Option<&str>,
    ) -> Vec<CompoundStatement> {
        self.stmt
            .remove_statement_recur_partial(attr_outer, attr_inner)
    }

    /// Appends `stmt` at the end of statements.
    /// If the last statement is [`Statement::Other`], `stmt` is appended to it.
    /// Otherwise, new `Other` is generated.
    pub fn append_statement(&mut self, stmt: String) {
        if let Some(Statement::Other(last_stmt)) = self.stmt.stmts.last_mut() {
            last_stmt.push(stmt);
        } else {
            self.stmt.stmts.push(Statement::Other(vec![stmt]));
        }
    }
}

impl ConstructWgslCode for WgslFn {
    fn write_wgsl_string(&self, buf: &mut String) {
        util::put_attrs(self.attrs.iter(), buf);
        buf.push_str("fn ");
        buf.push_str(self.ident.as_str());
        buf.push('(');
        util::put_str_join(self.inputs.iter(), buf, "", ",", "");
        buf.push(')');
        if let Some(output) = &self.output {
            buf.push_str("->");
            output.write_wgsl_string(buf);
        }
        self.stmt.write_wgsl_string(buf);
    }
}

impl ConstructPrettyCode for WgslFn {
    fn write_pretty_code(&self, buf: &mut String) {
        // Writes attributes.
        util::put_attrs_pretty(self.attrs.iter(), buf);
        buf.push('\n');

        // Writes from `fn` to the first input.
        let prev = buf.len();
        buf.push_str("fn ");
        buf.push_str(self.ident.as_str());
        buf.push('(');
        let input_tab = " ".repeat(buf.len() - prev);
        if let Some(first_input) = self.inputs.first() {
            first_input.write_pretty_code(buf);
            if self.inputs.len() > 1 {
                buf.push_str(",\n");
            }
        }

        // Writes other inputs.
        util::put_str_pretty_join(self.inputs.iter().skip(1), buf, &input_tab, ",\n", "");
        buf.push(')');

        // Writes output.
        if let Some(output) = &self.output {
            buf.push_str(" -> ");
            output.write_pretty_code(buf);
        }

        // Writes statement.
        buf.push('\n');
        util::pushn(buf, ' ', TAB_SIZE);
        self.stmt.write_pretty_code(buf);
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnParam {
    /// Attributes of the function parameter.
    ///
    /// e.g. fn @fragement fragementMain(**@location(0)** pos : vec3f, ...) -> **@location(0)** vec4f {...}
    pub attrs: Attributes,

    /// Name of the function parameter.
    ///
    /// e.g. fn foo(**name** : vec3f) {...}
    pub ident: Option<String>,

    /// type of the function parameter.
    ///
    /// e.g. fn foo(name : **vec3f**) -> **vec4f** {...}
    pub ty: String,
}

impl ConstructWgslCode for FnParam {
    fn write_wgsl_string(&self, buf: &mut String) {
        util::put_attrs(self.attrs.iter(), buf);
        if let Some(ident) = &self.ident {
            buf.push_str(ident);
            buf.push(':');
        }
        buf.push_str(&self.ty);
    }
}

impl ConstructPrettyCode for FnParam {
    fn write_pretty_code(&self, buf: &mut String) {
        util::put_attrs_pretty(self.attrs.iter(), buf);
        if let Some(ident) = &self.ident {
            buf.push_str(ident);
            buf.push_str(" : ");
        }
        buf.push_str(&self.ty);
    }
}

// 9.1. Compound Statement
// https://www.w3.org/TR/WGSL/#compound-statement-section
/// Compound statement is a set of statement wrapped with braces, {...}.
#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct CompoundStatement {
    /// Attributes of the statement.
    pub attrs: Attributes,

    /// Statements inside this block.
    pub stmts: Vec<Statement>,
}

impl CompoundStatement {
    /// Retrieve statements that have given attribute using recursive search.
    pub fn get_statement_recur(&self, attr: &Attribute) -> Vec<&CompoundStatement> {
        self.get_statement_recur_partial(attr.outer(), attr.inner().as_deref())
    }

    /// Searches partially matched attribute.
    /// If `inner` is Some, it tries to find exactly matched one.
    /// Otherwise, it compares outer only.
    pub fn get_statement_recur_partial(
        &self,
        outer: &str,
        inner: Option<&str>,
    ) -> Vec<&CompoundStatement> {
        let mut res = Vec::new();
        for stmt in self.stmts.iter() {
            if let Statement::Compound(comp_stmt) = stmt {
                if comp_stmt.attrs.contains_attribute_partial(outer, inner) {
                    // Captures only the most outer one.
                    res.push(comp_stmt);
                } else {
                    res.extend(comp_stmt.get_statement_recur_partial(outer, inner));
                }
            }
        }
        res
    }

    /// Retrieve statements that have given attribute using recursive search.
    pub fn get_statement_recur_mut(&mut self, attr: &Attribute) -> Vec<&mut CompoundStatement> {
        self.get_statement_recur_partial_mut(attr.outer(), attr.inner().as_deref())
    }

    /// Searches partially matched attribute.
    /// If `inner` is Some, it tries to find exactly matched one.
    /// Otherwise, it compares outer only.
    pub fn get_statement_recur_partial_mut(
        &mut self,
        outer: &str,
        inner: Option<&str>,
    ) -> Vec<&mut CompoundStatement> {
        let mut res = Vec::new();
        for stmt in self.stmts.iter_mut() {
            if let Statement::Compound(comp_stmt) = stmt {
                if comp_stmt.attrs.contains_attribute_partial(outer, inner) {
                    // Captures only the most outer one.
                    res.push(comp_stmt);
                } else {
                    res.extend(comp_stmt.get_statement_recur_partial_mut(outer, inner));
                }
            }
        }
        res
    }

    /// Removes statements that have given attribute using recursive search.
    /// Then returns removed statements.
    pub fn remove_statement_recur(&mut self, attr: &Attribute) -> Vec<CompoundStatement> {
        self.remove_statement_recur_partial(attr.outer(), attr.inner().as_deref())
    }

    /// Searches partially matched attribute.
    /// If `inner` is Some, it tries to find exactly matched one.
    /// Otherwise, it compares outer only.
    pub fn remove_statement_recur_partial(
        &mut self,
        outer: &str,
        inner: Option<&str>,
    ) -> Vec<CompoundStatement> {
        let mut removed = Vec::new();
        for i in (0..self.stmts.len()).rev() {
            if let Statement::Compound(comp_stmt) = &mut self.stmts[i] {
                if comp_stmt.attrs.contains_attribute_partial(outer, inner) {
                    if let Statement::Compound(removed_stat) = self.stmts.remove(i) {
                        removed.push(removed_stat);
                    }
                } else {
                    removed.extend(comp_stmt.remove_statement_recur_partial(outer, inner));
                }
            }
        }
        removed
    }

    /// Makes the specified statements bare statements.
    /// It finds all matching statements recursively even if the inside of matched statement.
    pub fn make_bare_statements_recur(&mut self, outer: &str, inner: Option<&str>) {
        for stmt in self.stmts.iter_mut() {
            match stmt {
                Statement::Compound(comp_stmt) | Statement::BareCompound(comp_stmt) => {
                    if comp_stmt.attrs.contains_attribute_partial(outer, inner) {
                        comp_stmt.make_bare_statements_recur(outer, inner);
                        let mut bare = Statement::BareCompound(comp_stmt.clone());
                        std::mem::swap(stmt, &mut bare);
                    } else {
                        comp_stmt.make_bare_statements_recur(outer, inner);
                    }
                }
                _ => (),
            }
        }
    }

    /// Writes only inner statements without attributes and braces.
    pub fn put_str_inner(&self, buf: &mut String) {
        util::put_str_join(self.stmts.iter(), buf, "", "", "");
    }

    /// Writes only inner statements without attributes and braces in pretty style.
    pub fn put_str_pretty_inner(&self, buf: &mut String) {
        let non_ws = util::get_last_whitespaces(buf);
        match buf.chars().nth_back(non_ws) {
            Some('=') | Some(',') => {
                util::popn(buf, non_ws);
                let indent = util::get_last_indent(buf);
                let indent = " ".repeat(indent);
                util::put_str_pretty_join(self.stmts.iter().take(1), buf, " ", "", "\n");
                util::put_str_pretty_join(self.stmts.iter().skip(1), buf, &indent, "", "");
            }
            _ => {
                let indent = util::get_last_indent(buf);
                let indent = " ".repeat(indent);
                util::put_str_pretty_join(self.stmts.iter().take(1), buf, "", "", "\n");
                util::put_str_pretty_join(self.stmts.iter().skip(1), buf, &indent, "", "");
            }
        }
    }
}

impl ConstructWgslCode for CompoundStatement {
    fn write_wgsl_string(&self, buf: &mut String) {
        util::put_str_join(self.attrs.iter(), buf, "", "", "");
        buf.push('{');
        util::put_str_join(self.stmts.iter(), buf, "", "", "");
        buf.push('}');
    }
}

impl ConstructPrettyCode for CompoundStatement {
    fn write_pretty_code(&self, buf: &mut String) {
        // Note that caller must have put a tab for this compound.
        let tab = util::get_last_spaces(buf);
        util::popn(buf, tab.min(TAB_SIZE));

        util::put_attrs_pretty(self.attrs.iter().filter(|attr| !attr.is_my_attr()), buf);
        buf.push_str("{\n");
        let tab_str = " ".repeat(tab);
        util::put_str_pretty_join(self.stmts.iter(), buf, &tab_str, "\n", "\n");
        util::pushn(buf, ' ', tab.saturating_sub(TAB_SIZE));
        buf.push('}');
    }
}

// 9.7. Statements Grammar Summary
// https://www.w3.org/TR/WGSL/#statements-summary
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Statement {
    // Not fully implemented.
    /// Compound statement is a set of statements wrapped with braces.
    Compound(CompoundStatement),

    /// Compound statement, but doesn't output attributes and braces.
    /// It's useful when you assign conditional compound statement to a variable.
    BareCompound(CompoundStatement),

    /// Currently, all statements without braces belong to this.
    Other(Vec<String>),
}

impl ConstructWgslCode for Statement {
    fn write_wgsl_string(&self, buf: &mut String) {
        match self {
            Self::Compound(comp_stmt) => comp_stmt.write_wgsl_string(buf),
            Self::BareCompound(comp_stmt) => comp_stmt.put_str_inner(buf),
            Self::Other(others) => {
                let mut prev = &String::from(".");
                for cur in others.iter() {
                    if cur.is_empty() {
                        continue;
                    }

                    let mut stick = false;
                    match unsafe { prev.as_str().chars().next().unwrap_unchecked() } {
                        '.' | ';' | '[' | '(' | '=' | '+' | '-' | '*' | '/' | ',' => {
                            stick = true;
                        }
                        _ => (),
                    }
                    match unsafe { cur.as_str().chars().next().unwrap_unchecked() } {
                        '.' | ';' | '[' | '(' | '=' | '+' | '-' | '*' | '/' | ',' => {
                            stick = true;
                        }
                        _ => {}
                    }
                    if !stick {
                        buf.push(' ');
                    }

                    buf.push_str(cur);
                    prev = cur;
                }
            }
        }
    }
}

impl ConstructPrettyCode for Statement {
    fn write_pretty_code(&self, buf: &mut String) {
        match self {
            Self::Compound(comp_stmt) => {
                util::pushn(buf, ' ', TAB_SIZE);
                comp_stmt.write_pretty_code(buf);
            }
            Self::BareCompound(comp_stmt) => comp_stmt.put_str_pretty_inner(buf),
            Self::Other(others) => {
                let mut prev = &String::from(".");
                for cur in others.iter() {
                    if cur.is_empty() {
                        continue;
                    }

                    let mut stick = false;
                    let (p_first, c_first) = unsafe {
                        (
                            prev.as_str().chars().next().unwrap_unchecked(),
                            cur.as_str().chars().next().unwrap_unchecked(),
                        )
                    };
                    match p_first {
                        '.' | ';' | '[' | '(' | ',' => {
                            stick = true;
                        }
                        _ => (),
                    }
                    match c_first {
                        '.' | ';' | '[' | '(' | ',' => {
                            stick = true;
                        }
                        _ => {}
                    }
                    if c_first == '=' && ['+', '-', '*', '/', '='].iter().any(|ch| *ch == p_first) {
                        stick = true;
                    }
                    if c_first == '(' && ['+', '-', '*', '/'].iter().any(|ch| *ch == p_first) {
                        stick = false;
                    }
                    if !stick {
                        buf.push(' ');
                    }

                    buf.push_str(cur);
                    prev = cur;
                }
            }
        }
    }
}
