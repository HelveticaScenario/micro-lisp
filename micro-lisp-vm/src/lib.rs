use std::cell::RefCell;
use std::cell::RefMut;
use std::collections::HashMap;
use std::rc::Rc;

mod tests;

#[derive(Debug, Clone, PartialEq)]
pub enum AST {
    Symbol(String),
    Str(String),
    Int(i64),
    Float(f64),
    Bool(bool),
    List(Vec<AST>),
    Nil,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Error {
    SyntaxError(Option<usize>),
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

type VMFunc = Box<dyn Fn(Vec<Val>) -> Val>;
// struct VMFuncObj {
//     func: VMFunc,
//     name: String,
// }

// impl Debug on VMFuncObj {

// }

pub enum Val {
    Ast(AST),
    Func(VMFunc),
    RuntimeError, // Func(fn(Vec<Val>) -> Val),
}

pub struct VM {
    fns: HashMap<String, Box<dyn Fn(Vec<Val>) -> Val>>,
    define_func: Option<Box<dyn Fn(String, Box<dyn Fn(Rc<RefCell<VM>>, Vec<Val>) -> Val>)>>,
}

impl VM {
    pub fn new() -> Rc<RefCell<VM>> {
        let fns = HashMap::new();
        let vm = Rc::new(RefCell::new(VM {
            fns: fns,
            define_func: None,
        }));
        let vm_clone = vm.clone();
        vm.borrow_mut().define_func = Some(Box::new(
            move |name, func: Box<dyn Fn(Rc<RefCell<VM>>, Vec<Val>) -> Val>| {
                let clone1 = vm_clone.clone();
                let clone2 = vm_clone.clone();
                clone1
                    .borrow_mut()
                    .fns
                    .insert(name, Box::new(move |ast| func(clone2.clone(), ast)));
            },
        ));
        // let insert = ;
        if let Some(define_func) = &vm.borrow_mut().define_func {
            define_func(
                "+".to_owned(),
                Box::new(|clone, ast| clone.borrow_mut().add(ast)),
            );
            define_func(
                "define".to_owned(),
                Box::new(|clone, ast| clone.borrow_mut().define(ast)),
            )
        }
        return vm;
    }
    fn define(&mut self, args: Vec<Val>) -> Val {
        if let Some(define_func) = &self.define_func {
            return Val::Ast(AST::Nil);
        } else {
            return Val::RuntimeError;
        }
    }

    fn add(&mut self, args: Vec<Val>) -> Val {
        if args.len() < 2 {
            Val::RuntimeError
        } else if let Some(Val::Ast(AST::Int(_))) = args.first() {
            VM::add_ints(args)
        } else if let Some(Val::Ast(AST::Float(_))) = args.first() {
            VM::add_floats(args)
        } else {
            Val::RuntimeError
        }
    }
    fn add_ints(args: Vec<Val>) -> Val {
        let mut sum = 0i64;
        for val in args.iter() {
            if let Val::Ast(AST::Int(i)) = val {
                sum += i;
            } else {
                return Val::RuntimeError;
            }
        }
        return Val::Ast(AST::Int(sum));
    }
    fn add_floats(args: Vec<Val>) -> Val {
        let mut sum = 0f64;
        for val in args.iter() {
            if let Val::Ast(AST::Float(i)) = val {
                sum += i;
            } else {
                return Val::RuntimeError;
            }
        }
        return Val::Ast(AST::Float(sum));
    }
    pub fn exec(&mut self, code: String) -> Val {
        Val::Ast(AST::Nil)
    }
}

pub fn hello() {
    println!("hello lib");
}
