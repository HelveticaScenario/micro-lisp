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
use std::cell::RefMut;
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
        let define_internal = move |name, func: Box<dyn Fn(Rc<RefCell<VM>>, &[Val]) -> Val>| {
            let inner_clone = vm_clone.clone();
            if let Some(scope) = vm_clone.borrow_mut().env.last_mut() {
                scope.insert(
                    name,
                    Val::Proc(Procedure::new(
                        name.clone(),
                        Box::new(move |ast| func(inner_clone.clone(), ast)),
                    )),
                );
            }
        };

        define_internal(
            "+".to_owned(),
            Box::new(|clone, ast| clone.borrow().add(ast)),
        );
        define_internal(
            "define".to_owned(),
            Box::new(|clone, ast| clone.borrow_mut().define(ast)),
        );
        return vm;
    }

    fn define(&mut self, args: &[Val]) -> Val {
        return Val::RuntimeError;
    }

    fn add(&self, args: &[Val]) -> Val {
        if args.len() < 2 {
            Val::RuntimeError
        } else if let Some(Val::Int(_)) = args.first() {
            VM::add_ints(args)
        } else if let Some(Val::Float(_)) = args.first() {
            VM::add_floats(args)
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
    fn search_env(&mut self, name: &String) -> Option<Val> {
        for env in self.env.iter().rev() {
            if let Some(val) = env.get(name) {
                return Some(val.clone());
            }
        }
        return None;
    }

    pub fn eval(&mut self, arg: &Val) -> Val {
        match arg {
            Val::Symbol(name) => self.search_env(name).unwrap_or(Val::RuntimeError),
            Val::List(list) => {
                if let Some(Val::Proc(procedure)) = list.first() {
                    procedure.call(&list[1..])
                } else {
                    Val::RuntimeError
                }
            }
            _ => arg.clone(),
        }
    }

    pub fn parse(code: String) -> Result<Vec<AST>, Error> {
        let mut ast_stack: Vec<AST> = vec![AST::List(vec![])];
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
                    if let Some(AST::Str(s)) = ast_stack.last_mut() {
                        s.push(character);
                        continue;
                    }
                    if let Some(AST::Symbol(_)) = ast_stack.last() {
                        let sym = ast_stack.pop().unwrap();
                        if let Some(AST::List(list)) = ast_stack.last_mut() {
                            list.push(sym);
                        }
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
                                    return err;
                                }
                            }
                            AST::Bool(_) | AST::Float(_) | AST::Int(_) | AST::Nil => {
                                if let Some(AST::List(list)) = ast_stack.last_mut() {
                                    list.push(ast);
                                } else {
                                    return err;
                                }
                            }
                            AST::List(_) => {
                                ast_stack.push(ast);
                            }
                        }
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

    pub fn exec(&mut self, code: String) -> Result<Val, Error> {
        match VM::parse(code) {
            Ok(ast) => {
                let vals = ast.iter().map(|ast| self.eval(&Val::from(ast))).collect();
                if vals.len() == 1 {
                    return Ok(vals.first().unwrap());
                } else {
                    return Ok(Val::List(vals));
                }
            }
            e => e,
        }
    }
}

pub fn hello() {
    println!("hello lib");
}
