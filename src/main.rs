use std::collections::HashMap;

use pest::{iterators::Pair, Parser};
use pest_derive::Parser;

const BUILTIN: [&str;1] = ["print"];

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct Lexer;

fn main() {
    let res = parsing("x=4 * 2;print(x);");
    if res.is_err(){
        println!("Error: {}", res.unwrap_err());
    }
    else {
        let res = run_ir(res.unwrap());
        if res.is_err(){println!("{}", res.unwrap_err());}
    }
}

#[derive(Debug, Clone)]
enum Operation{
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl Operation{
    fn parse(input: &str) -> Option<Operation>{
        match input{
            "+" => {
                return Some(Operation::Add);
            }
            "-" => {
                return Some(Operation::Subtract);
            }
            "*" => {
                return Some(Operation::Multiply);
            }
            "/" => {
                return Some(Operation::Divide);
            }
            _ => {
                return None;
            }
        }
    }

    fn run(&self, left: &Box<Data>, right: &Box<Data>, varibles: &HashMap<String, Data>) -> Option<String>{
        match self{
            Operation::Add => {
                match *left.clone(){
                    Data::Number(value1) => {
                        match *right.clone(){
                            Data::Number(value2) => {
                                return Some((value1 + value2).to_string());
                            }
                            Data::String(value2) => {
                                return Some(value1.to_string() + value2.as_str());
                            }
                            _ => {
                                return None;
                            }
                        }
                    }
                    Data::String(value1) => {
                        let right = right.evaluate(varibles);
                        if right.is_none(){return None;}
                        return Some(value1 + right.unwrap().as_str());
                    }
                    _ => {
                        return None;
                    }
                }
            }
            Operation::Subtract => {
                match **left{
                    Data::Number(value1) => {
                        match **right{
                            Data::Number(value2) => {
                                return Some((value1 - value2).to_string());
                            }
                            _ => {
                                return None;
                            }
                        }
                    }
                    _ => {
                        return None;
                    }
                }
            }
            Operation::Multiply => {
                match **left{
                    Data::Number(value1) => {
                        match **right{
                            Data::Number(value2) => {
                                return Some((value1 * value2).to_string());
                            }
                            _ => {
                                return None;
                            }
                        }
                    }
                    _ => {
                        return None;
                    }
                }
            }
            Operation::Divide => {
                match **left{
                    Data::Number(value1) => {
                        match **right{
                            Data::Number(value2) => {
                                return Some((value1 / value2).to_string());
                            }
                            _ => {
                                return None;
                            }
                        }
                    }
                    _ => {
                        return None;
                    }
                }
            }
            
        }
    }
}
#[derive(Debug, Clone)]
enum Data{
    String(String),
    Number(f64),
    MathExpression(Box<Data>, Operation, Box<Data>),
    Variable(String),
}

impl Data{
    fn parse(input: &Pair<'_, Rule>) -> Option<Data>{
        match input.as_rule(){
            Rule::number => {
                let value = input.clone().into_inner().peek();
                if value.is_none(){return None;}
                else{
                    let value: Result<f64, _> = value.unwrap().as_str().parse();
                    if value.is_err(){return None;}
                    return Some(Data::Number(value.unwrap()));
                }
            }
            Rule::string => {
                let value = input.clone().into_inner().peek();
                if value.is_none(){return None;}
                else{
                    let value = value.unwrap().as_str().replace('"', "").replace("'", "");
                    return Some(Data::String(value));
                }
            }
            Rule::math_expression => {
                let mut expression = input.clone().into_inner();
                let left = Data::parse(&expression.next().unwrap());
                let operation = Operation::parse(expression.next().unwrap().as_str());
                if operation.is_none() || left.is_none(){return None;}
                let right = Data::parse(&expression.next().unwrap());
                if left.is_none() || right.is_none(){return None;}
                else{
                    return Some(Data::MathExpression(Box::new(left.unwrap()), operation.unwrap(), Box::new(right.unwrap())));
                }
            }
            Rule::identifier => {
                return Some(Data::Variable(input.as_str().to_string()));
            }
            _ => {
                return None;
            }
        }
    }

    fn evaluate(&self, variables: &HashMap<String, Data>) -> Option<String>{
        match self{
            Data::Number(value) => {
                return Some(value.to_string());
            }
            Data::String(value) => {
                return Some(value.to_string());
            }
            Data::MathExpression(left, operation, right) => {
                return operation.run(left, right, variables)
            }
            Data::Variable(value) => {
                let value = variables.get(value);
                if value.is_none(){return None;}
                else{
                    return value.unwrap().evaluate(variables);
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
enum IR{
    Function(String, Vec<Data>),
    Assignment(String, Data),
}

fn parsing(input: &str) -> Result<Vec<IR>, String> {
    let result = Lexer::parse(Rule::program, input);
    if result.is_err(){
        let error = result.unwrap_err();
        return Err(format!("on Line: {:?} -> Syntax Error: {} -> {}", error.line_col, error.variant.message(), error.line()));
    }
    else{
        let mut tokens: Vec<IR> = Vec::new();
        let statements = result.unwrap().peek().unwrap().into_inner();
        for statement in statements.into_iter(){
            if statement.as_rule() != Rule::statement{continue;}
            let statement =  statement.into_inner().peek().unwrap();
            let line =  statement.line_col().0;
            match statement.as_rule(){
                Rule::function_call => {
                    let mut function_name = "";
                    let mut args = Vec::new();
                    if statement.clone().into_inner().peek().is_none(){tokens.push(IR::Function(function_name.to_string(), args));continue;}
                    for (i, arg) in statement.into_inner().into_iter().into_iter().enumerate(){
                        let line = (arg.as_str(), line);
                        if i == 0{function_name = arg.as_str();}
                        else {
                            let data = Data::parse(&arg.into_inner().peek().unwrap());
                            if data.is_none(){
                                return Err("Invalid argument: ".to_string() +
                                line.0 + " On line: " + 
                                line.1.to_string().as_str());
                            }
                            
                            args.push(data.unwrap());
                        }
                    }
                    tokens.push(IR::Function(function_name.to_string(), args));
                }
                Rule::math_expression => {
                    continue;
                }
                Rule::assignment => {
                    let mut statement = statement.into_inner();
                    let var = statement.next().unwrap();
                    let value = statement.next().unwrap();
                    let data = Data::parse(&value);
                    if data.is_none(){
                        return Err("Invalid argument: ".to_string() +
                        var.as_str() + " On line: " + 
                        line.to_string().as_str());
                    }
                    tokens.push(IR::Assignment(var.as_str().to_string(), data.unwrap()));
                }
                _ => {
                    continue;
                }
            }
        }
        return Ok(tokens);
    }
}

fn call_builtin(name: &str, args: &Vec<Data>, variables: &HashMap<String, Data>) -> Result<(), String>{
    if !BUILTIN.contains(&name){return Err("NO FUNCTION FOUND WITH THIS NAME".to_string());}
    else{
        match name{
            "print" => {
                for arg in args.iter(){
                    let arg = arg.evaluate(variables);
                    if arg.is_none(){return Err("INVALID ARGUMENT".to_string());}
                    println!("{}", arg.unwrap());
                }
            }
            _ => {
                return Err("NO FUNCTION FOUND WITH THIS NAME".to_string());
            }
        }
    }
    return Ok(());
}

fn run_ir(tokens: Vec<IR>) -> Result<(), String>{
    let mut variables: HashMap<String, Data>  = HashMap::new();
    for token in tokens.iter(){
        match token{
            IR::Function(name, args) => {
                let err = call_builtin(name, args, &variables);
                if err.is_err(){return err;}
            }
            IR::Assignment(var, data) => {
                variables.insert(var.to_string(), data.clone());
            }
        }
    }
    return Ok(());
}