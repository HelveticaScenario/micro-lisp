/*
const VM = {
  fns: {},
  define_function: (name, func) => {
    VM.fns[name] = (ast) => func(VM, ast)
  }
}
*/
extern crate rand;

mod tests;
mod types;

use crate::types::{Error, Procedure, Val, AST};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct VM {
    env: Vec<HashMap<String, Val>>,
}

impl VM {
    pub fn new() -> Rc<RefCell<VM>> {
        let env = vec![HashMap::new()];

        let vm = Rc::new(RefCell::new(VM { env: env }));
        let vm_clone = vm.clone();
        let define_internal = move |name: &String, func: Box<dyn Fn(&mut VM, &[Val]) -> Val>| {
            if let Some(scope) = vm_clone.borrow_mut().env.last_mut() {
                scope.insert(name.clone(), Val::Proc(Procedure::new(name.clone(), func)));
            }
        };

        define_internal(&"+".to_owned(), Box::new(|vm: &mut VM, ast| vm.add(ast)));
        define_internal(&"-".to_owned(), Box::new(|vm: &mut VM, ast| vm.sub(ast)));
        define_internal(&"*".to_owned(), Box::new(|vm: &mut VM, ast| vm.mul(ast)));
        define_internal(&"/".to_owned(), Box::new(|vm: &mut VM, ast| vm.div(ast)));
        define_internal(
            &"define".to_owned(),
            Box::new(|vm: &mut VM, ast| vm.define(ast)),
        );
        return vm;
    }

    fn define(&mut self, args: &[Val]) -> Val {
        if args.len() != 2 {
            Val::RuntimeError
        } else if let (Some(Val::Symbol(name)), val) = (args.get(0), args.get(1)) {
            if let Some(res) = self.eval(val) {
                if let Some(env) = self.env.last_mut() {
                    env.insert(name.clone(), res);
                    Val::Nil
                } else {
                    Val::RuntimeError
                }
            } else {
                Val::RuntimeError
            }
        } else {
            Val::RuntimeError
        }
    }

    fn add(&mut self, args: &[Val]) -> Val {
        if args.len() < 2 {
            return Val::RuntimeError;
        }
        let evaluated: Vec<Val> = args.iter().map(|arg| self.eval(Some(arg)).unwrap()).collect();
        if let Some(Val::Int(_)) = evaluated.first() {
            VM::add_ints(&evaluated)
        } else if let Some(Val::Float(_)) = evaluated.first() {
            VM::add_floats(&evaluated)
        } else {
            Val::RuntimeError
        }
    }
    fn add_ints(args: &[Val]) -> Val {
        let mut sum = 0i64;
        for val in args.iter() {
            if let Val::Int(i) = val {
                sum += i;
            } else {
                return Val::RuntimeError;
            }
        }
        return Val::Int(sum);
    }
    fn add_floats(args: &[Val]) -> Val {
        let mut sum = 0f64;
        for val in args.iter() {
            if let Val::Float(i) = val {
                sum += i;
            } else {
                return Val::RuntimeError;
            }
        }
        return Val::Float(sum);
    }
    fn sub(&mut self, args: &[Val]) -> Val {
        if args.len() < 2 {
            return Val::RuntimeError
        } 
        let evaluated: Vec<Val> = args.iter().map(|arg| self.eval(Some(arg)).unwrap()).collect();
        if let Some(Val::Int(_)) = evaluated.first() {
            VM::sub_ints(&evaluated)
        } else if let Some(Val::Float(_)) = evaluated.first() {
            VM::sub_floats(&evaluated)
        } else {
            Val::RuntimeError
        }
    }
    fn sub_ints(args: &[Val]) -> Val {
        let mut res = 0i64;
        for val in args.iter() {
            if let Val::Int(i) = val {
                res -= i;
            } else {
                return Val::RuntimeError;
            }
        }
        return Val::Int(res);
    }
    fn sub_floats(args: &[Val]) -> Val {
        let mut res = 0f64;
        for val in args.iter() {
            if let Val::Float(i) = val {
                res -= i;
            } else {
                return Val::RuntimeError;
            }
        }
        return Val::Float(res);
    }
    fn mul(&mut self, args: &[Val]) -> Val {
        if args.len() < 2 {
            return Val::RuntimeError
        } 
        let evaluated: Vec<Val> = args.iter().map(|arg| self.eval(Some(arg)).unwrap()).collect();
        if let Some(Val::Int(_)) = evaluated.first() {
            VM::mul_ints(&evaluated)
        } else if let Some(Val::Float(_)) = evaluated.first() {
            VM::mul_floats(&evaluated)
        } else {
            Val::RuntimeError
        }
    }
    fn mul_ints(args: &[Val]) -> Val {
        let mut res = 1i64;
        for val in args.iter() {
            if let Val::Int(i) = val {
                res *= i;
            } else {
                return Val::RuntimeError;
            }
        }
        return Val::Int(res);
    }
    fn mul_floats(args: &[Val]) -> Val {
        let mut res = 1f64;
        for val in args.iter() {
            if let Val::Float(i) = val {
                res *= i;
            } else {
                return Val::RuntimeError;
            }
        }
        return Val::Float(res);
    }
    fn div(&mut self, args: &[Val]) -> Val {
        if args.len() < 2 {
            return Val::RuntimeError
        } 
        let evaluated: Vec<Val> = args.iter().map(|arg| self.eval(Some(arg)).unwrap()).collect();
        if let Some(Val::Int(_)) = evaluated.first() {
            VM::div_ints(&evaluated)
        } else if let Some(Val::Float(_)) = evaluated.first() {
            VM::div_floats(&evaluated)
        } else {
            Val::RuntimeError
        }
    }
    fn div_ints(args: &[Val]) -> Val {
        if let Some(Val::Int(first)) = args.first() {
            let mut res = *first;
            for val in args[1..].iter() {
                if let Val::Int(i) = val {
                    res /= i;
                } else {
                    return Val::RuntimeError;
                }
            }
            return Val::Int(res);
        } else {
            Val::RuntimeError
        }
    }
    fn div_floats(args: &[Val]) -> Val {
        if let Some(Val::Float(first)) = args.first() {
            let mut res = *first;
            for val in args[1..].iter() {
                if let Val::Float(i) = val {
                    res /= i;
                } else {
                    return Val::RuntimeError;
                }
            }
            return Val::Float(res);
        } else {
            Val::RuntimeError
        }
    }
    fn search_env(&mut self, name: &String) -> Option<Val> {
        for env in self.env.iter().rev() {
            if let Some(val) = env.get(name) {
                return Some(val.clone());
            }
        }
        return None;
    }

    pub fn eval(&mut self, arg: Option<&Val>) -> Option<Val> {
        if let Some(val) = arg {
            match val {
                Val::Symbol(name) => Some(self.search_env(name).unwrap_or(Val::RuntimeError)),
                Val::List(list) => {
                    if let Some(Val::Proc(procedure)) = self.eval(list.first()) {
                        Some(procedure.call(self, &list[1..]))
                    } else {
                        Some(Val::RuntimeError)
                    }
                }
                _ => Some(val.clone()),
            }
        } else {
            None
        }
    }

    pub fn parse(code: &String) -> Result<Vec<AST>, Error> {
        let mut ast_stack: Vec<AST> = vec![AST::List(vec![])];
        let pop_parse_push = |character, ast_stack: &mut Vec<AST>| {
            if let Some(ast) = ast_stack.pop() {
                match ast {
                    AST::Str(mut s) => {
                        s.push(character);
                        ast_stack.push(AST::Str(s));
                    }
                    AST::Symbol(sym) => {
                        if let Some(AST::List(list)) = ast_stack.last_mut() {
                            list.push(if sym == "nil" {
                                AST::Nil
                            } else if let Ok(b) = sym.parse::<bool>() {
                                AST::Bool(b)
                            } else if let Ok(i) = sym.parse::<i64>() {
                                AST::Int(i)
                            } else if let Ok(f) = sym.parse::<f64>() {
                                AST::Float(f)
                            } else {
                                AST::Symbol(sym)
                            });
                        } else {
                            return Err(());
                        }
                    }
                    AST::Bool(_) | AST::Float(_) | AST::Int(_) | AST::Nil => {
                        if let Some(AST::List(list)) = ast_stack.last_mut() {
                            list.push(ast);
                        } else {
                            return Err(());
                        }
                    }
                    AST::List(_) => {
                        ast_stack.push(ast);
                    }
                }
            }
            return Ok(());
        };
        let mut escaped: bool = false;
        for (idx, character) in code.chars().enumerate() {
            let err = Err(Error::SyntaxError(Some(idx)));
            match (character, escaped) {
                ('(', false) => {
                    // dont allow new list if currently in a symbol
                    if let Some(AST::Str(s)) = ast_stack.last_mut() {
                        s.push(character);
                    } else if let Some(AST::List(_)) = ast_stack.last() {
                        ast_stack.push(AST::List(vec![]));
                    } else {
                        return err;
                    }
                }
                (')', false) => {
                    if let Err(_) = pop_parse_push(character, &mut ast_stack) {
                        return err;
                    }
                    // there is an implicit root list, so if we try to close a list and all we have is the
                    // root then there is a syntax error
                    if ast_stack.len() == 1 {
                        return err;
                    }
                    if let Some(AST::List(list)) = ast_stack.pop() {
                        if let Some(AST::List(container)) = ast_stack.last_mut() {
                            container.push(AST::List(list))
                        } else {
                            return err;
                        }
                    } else {
                        return err;
                    }
                }
                ('"', false) => {
                    if let Some(AST::Str(_)) = ast_stack.last() {
                        let s = ast_stack.pop().unwrap();
                        if let Some(AST::List(list)) = ast_stack.last_mut() {
                            list.push(s);
                        } else {
                            return err;
                        }
                    } else if let Some(AST::List(_)) = ast_stack.last() {
                        ast_stack.push(AST::Str("".to_owned()));
                    } else {
                        return err;
                    }
                }
                ('\\', false) => {
                    escaped = true;
                }
                (' ', _) | ('\n', _) | ('\t', _) | ('\r', _) => {
                    if let Err(_) = pop_parse_push(character, &mut ast_stack) {
                        return err;
                    }
                }
                _ => {
                    let c = if escaped {
                        match character {
                            'b' => '\u{0008}',
                            'f' => '\u{000C}',
                            'n' => '\n',
                            'r' => '\r',
                            't' => '\t',
                            '\'' => '\'',
                            '\"' => '\"',
                            '\\' => '\\',
                            _ => character,
                        }
                    } else {
                        character
                    };
                    if let Some(AST::Symbol(s)) | Some(AST::Str(s)) = ast_stack.last_mut() {
                        s.push(c);
                    } else if let Some(AST::List(_)) = ast_stack.last() {
                        ast_stack.push(AST::Symbol(String::from(c)))
                    }
                    escaped = false;
                }
            }
        }
        if ast_stack.len() != 1 {
            return Err(Error::SyntaxError(None));
        }
        if let Some(AST::List(root)) = ast_stack.pop() {
            return Ok(root);
        }
        return Err(Error::SyntaxError(None));
    }

    pub fn exec(&mut self, code: &String) -> Result<Val, Error> {
        match VM::parse(code) {
            Ok(ast) => {
                let vals: Vec<Val> = ast
                    .iter()
                    .map(|ast| self.eval(Some(&Val::from(ast))).unwrap_or(Val::Nil))
                    .collect();
                if vals.len() == 1 {
                    return Ok(vals.first().unwrap().clone());
                } else {
                    return Ok(Val::List(vals));
                }
            }
            Err(e) => Err(e),
        }
    }
}
