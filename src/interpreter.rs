use trunk_lexer::{ByteString, Lexer};
use trunk_parser::{Parser, Statement, Expression, Block};
use mysql_async::prelude::*;
use mysql_async::from_row;
use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use regex::Regex;
use crate::app_state::*;
use crate::mixnmatch::*;


#[derive(Debug)]
pub enum PhpError {
    NoSuchCodeFragment(usize),
    NotImplemented,
    NoSuchArg,
    RegexError(String),
    NoSuchVariable(String),
    NoSuchProperty(String),
    WrongVariableType(String),
    NoArrayIndex,
}

impl Error for PhpError {}

impl fmt::Display for PhpError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self) // user-facing output
    }
}

#[derive(Debug, Clone)]
pub enum PhpVariable {
    Null,
    String {
        value: String
    },
    NumberedArray {
        elements: Vec<Box<PhpVariable>>
    },
    AssociativeArray {
        elements: HashMap<String,Box<PhpVariable>>
    },
    Object {
        elements: HashMap<String,Box<PhpVariable>>
    },
    Integer {
        value: i64
    },
    Float {
        value: f64
    },
    Bool {
        value: bool
    }
}

impl PhpVariable {
    pub fn to_string(&self) -> String {
        match self {
            PhpVariable::String { value } => value.to_owned(),
            PhpVariable::Bool { value } => format!("{}",value),
            other => format!("{:?}",other)
        }
    }

    pub fn to_i64(&self) -> i64 {
        match self {
            PhpVariable::Integer { value } => *value,
            _ => 0 // TODO more
        }
    }
}

#[derive(Debug, Clone)]
pub struct PhpFragment {
    mnm: MixNMatch,
    //php_code: String,
    ast: Block,
    vars: HashMap<String,PhpVariable>,
}

impl PhpFragment {
    pub async fn new_from_code_fragment_id(code_fragment_id: usize, mnm: &MixNMatch) -> Result<Self,GenericError> {
        let sql = "SELECT `php` FROM `code_fragments` WHERE `id`=:code_fragment_id" ;
        let php = mnm.app.get_mnm_conn().await?
            .exec_iter(sql.clone(),params! {code_fragment_id}).await?
            .map_and_drop(from_row::<String>).await?;
        let php_code = php.get(0).ok_or_else(||PhpError::NoSuchCodeFragment(code_fragment_id))?.to_owned();
        let php_code = format!("<?php\n{}",php_code);
        println!("{}",&php_code);

        let mut lexer = Lexer::new(None);
        let tokens = lexer.tokenize(&php_code[..]).unwrap();
        //println!("{:#?}",&tokens);
        
        let mut parser = Parser::new(None);
        let ast = parser.parse(tokens).unwrap();

        let ret = Self{
            mnm: mnm.clone(),
            //php_code,
            ast,
            vars: HashMap::new(),
        };
        Ok(ret)
    }

    fn expression_as_variable(&self, expression: &Expression) -> Result<String,PhpError> {
        match expression {
            Expression::Variable { name } => Ok(self.bs2s(name)),
            Expression::Identifier { name } => Ok(self.bs2s(name)),
            other => {
                println!("expression_as_variable: {:?}",&other);
                todo!()
                //Err(PhpError::NotImplemented)
            }
        }
    }

    fn expression_as_string(&self,expression: Option<&Expression>) -> Result<String,PhpError> {
        let expression = expression.ok_or_else(||PhpError::NoSuchArg)?;
        match expression {
            Expression::ConstantString{value} => Ok(self.bs2s(&value)),
            Expression::Int { i } => Ok(format!("{i}")),
            Expression::Float { f } => Ok(format!("{f}")),
            //Expression::Variable { name } => Ok(self.bs2s(name)),
            Expression::PropertyFetch { target, property } => {
                let var_name = self.expression_as_variable(target.as_ref())?;
                let property = self.expression_as_variable(property.as_ref())?;
                let variable = match self.vars.get(&var_name) {
                    Some(variable) => variable,
                    None => return Err(PhpError::NoSuchVariable(var_name))
                };
                match variable {
                    PhpVariable::Object { elements } => {
                        match elements.get(&property) {
                            Some(value) => Ok(value.to_string()),
                            None => Err(PhpError::NoSuchProperty(property))
                        }
                    }
                    _ => Err(PhpError::WrongVariableType(var_name))
                }
            }
            other => {
                println!("expression_as_string: {:?}",&other);
                todo!()
                //Err(PhpError::NotImplemented)
            }
        }
    }

    fn string2regex(&self, s: &str) -> Result<Regex,PhpError> {
        // TODO : /g, /i etc.
        // TODO cache
        let s = s[1..s.len()-1].to_string();
        Regex::new(&s).map_err(|_|PhpError::RegexError(s))
    }

    fn preg_match(&mut self, args: &Vec<Expression>) -> Result<Expression,PhpError> {
        let pattern = self.expression_as_string(args.get(0))?;
        let regex = self.string2regex(&pattern)?;
        println!("Args: {:?}",&args);
        println!("Regex: {:?}",&regex);
        let text = self.expression_as_string(args.get(1))?;
        println!("Value: {}",&text);
        
        if args.len()==2 { // Match only
            return Ok(Expression::Bool { value: regex.is_match(&text) });
        }

        let output_var_name_opt = match args.get(2) {
            Some(expression) => {
                match expression {
                    Expression::Variable { name } => Some(self.bs2s(&name)),
                    _ => None
                }
            }
            None => None
        };

        if let Some(output_var_name) = &output_var_name_opt {
            // TODO remove variable?
        }

        let captures = match regex.captures(&text) {
            Some(c) => c,
            None => return Ok(Expression::Bool{value:false})
        };

        if let Some(output_var_name) = &output_var_name_opt {
            println!("Writing to variable {}",&output_var_name);
            let values: Vec<Box<PhpVariable>> = captures
                .iter()
                .map(|c|{
                    match c {
                        Some(c) => c.as_str().to_string(),
                        None => String::new()
                    }
                })
                .map(|s|PhpVariable::String { value: s })
                .map(|v|Box::new(v))
                .collect();
            println!("Values: {:?}",&values);
            self.vars.insert(output_var_name.to_owned(), PhpVariable::NumberedArray { elements: values });
        }

        Ok(Expression::Bool{value:true})
    }

    fn method(&mut self, method: &str, args: &Vec<Expression>) -> Result<Expression,PhpError> {
        match method {
            "preg_match" => self.preg_match(args),
            other => {
                println!("method: '{}' not implemented",other);
                todo!()
                //Err(PhpError::NotImplemented)
            }
        }
    }

    fn expression_as_bool(&self, expression: &Expression) -> bool {
        match expression {
            Expression::Bool{value} => *value,
            Expression::Int{i} => *i!=0, // TODO check
            Expression::Float{f} => *f!=0.0, // TODO check
            Expression::ConstantString{value} => !value.is_empty(), // TODO also not "0"
            Expression::Array{items} => !items.is_empty(),
            _ => false
        }
    }

    fn bs2s(&self, bs: &ByteString) -> String {
        String::from_utf8(bs.to_vec()).unwrap()
    }

    fn check_condition(&mut self, expression: &Expression) -> Result<bool,PhpError> {
        match expression {
            Expression::Call{target,args} => {
                match target.as_ref() {
                    Expression::Identifier{name} => {
                        let method = self.bs2s(name);
                        println!("Calling {}",&method);
                        let args_exp = args.iter().map(|arg|arg.value.clone()).collect();
                        let result = self.method(&method,&args_exp)?;
                        return Ok(self.expression_as_bool(&result));
                    }
                    other => {
                        println!("{:?}",&other);
                        todo!()
                        //return Err(PhpError::NotImplemented)
                    }
                }
            }
            other => {
                println!("check_condition: {:?}",&other);
                todo!()
                //return Err(PhpError::NotImplemented)
            }
        }
    }

    fn calculate_expression_as_variable(&self, expression: &Expression) -> Result<PhpVariable,PhpError> {
        println!("calculate_expression_as_variable: {:?}",&expression);
        match expression {
            Expression::ArrayIndex { array, index } => {
                let var_name = self.expression_as_variable(array.as_ref())?;
                match index {
                    Some(index) => {
                        let index = self.calculate_expression_as_variable(index.as_ref())?;
                        let index = index.to_i64() as usize;
                        match self.vars.get(&var_name) {
                            Some(v) => {
                                match v {
                                    PhpVariable::NumberedArray { elements } => {
                                        return match elements.get(index).as_ref() {
                                            Some(v) => Ok(v.as_ref().to_owned()),
                                            None => Ok(PhpVariable::Null)
                                        };
                                    }
                                    other => {
                                        return Err(PhpError::WrongVariableType(format!("{:?}",other)));
                                    }
                                }
                            },
                            None => return Ok(PhpVariable::Null)
                        }
                    }
                    None => {
                        return Err(PhpError::NoArrayIndex);
                    }
                }
            }
            other => {
                println!("calculate_expression_as_variable: {:?}",&other);
                todo!()
            }
        }
        Err(PhpError::NoSuchArg)
    }
 
    pub fn run(&mut self, ast: &Block) -> Result<(),PhpError> {
        //println!("RUNNING {:?}",ast);
        for statement in ast {
            match statement {
                Statement::If{condition,then,else_ifs,r#else} => {
                    if self.check_condition(condition)? {
                        println!("THEN: {:?}",&then);
                        self.run(&then)? ;
                    // TODO else_ifs
                    } else {
                        if let Some(x) = r#else { self.run(x)? } ;
                    }
                }
                Statement::Expression { expr } => {
                    match expr {
                        Expression::Infix { lhs, op, rhs } => {
                            let value = self.calculate_expression_as_variable(&rhs)?;
                            
                        }
                        other => {
                            println!("run(Expression): {:?}",&other);
                            todo!()        
                        }
                    }
                }
                other => {
                    println!("run: {:?}",&other);
                    todo!()
                    //return Err(PhpError::NotImplemented);
                }
            }
        }
        Ok(())
    }

}



#[cfg(test)]
mod tests {

    use super::*;

    #[tokio::test]
    async fn test_new_from_code_fragment_id() {
        let mnm = get_test_mnm();
        let mut pf = PhpFragment::new_from_code_fragment_id(2, &mnm).await.unwrap();
        println!("{:#?}",&pf.ast);
        let elements = [
            ["ext_desc","(1921-2345)"],
        ];
        let elements = elements.iter().map(|v|{
            (v[0].to_string(),Box::new(PhpVariable::String{value:v[1].to_string()}))
        }).collect();
        pf.vars.insert("o".to_string(),PhpVariable::Object{elements});
        println!("Vars: {:#?}",&pf.vars);
        pf.run(&pf.ast.clone()).unwrap();
    }
}