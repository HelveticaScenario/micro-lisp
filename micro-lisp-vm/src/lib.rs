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

use crate::types::{Env, Error, Procedure, Val, AST};
use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

pub struct VM {
    env: Rc<Box<Env>>,
}

impl VM {
    fn get_root_env(&self) -> Rc<Box<Env>> {
        let mut last = self.env.clone();
        while let Some(parent) = &last.parent {
            last = parent.clone();
        }
        return last;
    }
}

impl VM {
    pub fn new() -> Rc<RefCell<VM>> {
        let vm = Rc::new(RefCell::new(VM {
            env: Rc::new(Box::new(Env::new(None))),
        }));
        let vm_clone = vm.clone();
        let define_internal = move |name: &String, func: Box<dyn Fn(&mut VM, &[Val], Rc<Box<Env>>) -> Val>| {
            vm_clone
                .borrow()
                .get_root_env()
                .set_val(name, Val::Proc(Procedure::new(name.clone(), func)))
        };

        define_internal(
            &"+".to_owned(),
            Box::new(|vm: &mut VM, ast, env| vm.add(ast, &env)),
        );
        define_internal(
            &"-".to_owned(),
            Box::new(|vm: &mut VM, ast, env| vm.sub(ast, &env)),
        );
        define_internal(
            &"*".to_owned(),
            Box::new(|vm: &mut VM, ast, env| vm.mul(ast, &env)),
        );
        define_internal(
            &"/".to_owned(),
            Box::new(|vm: &mut VM, ast, env| vm.div(ast, &env)),
        );
        define_internal(
            &"define".to_owned(),
            Box::new(|vm: &mut VM, ast, env| vm.define(ast, &env)),
        );
        define_internal(
            &"lambda".to_owned(),
            Box::new(|vm: &mut VM, ast, env| vm.lambda(ast, &env)),
        );
        return vm;
    }

    fn define(&mut self, args: &[Val], env: &Rc<Box<Env>>) -> Val {
        if !self.env.is_root() {
            Val::RuntimeError(Error::DefineNotAtRoot)
        } else if args.len() != 2 {
            Val::RuntimeError(Error::IncorrectNumberOfArguments)
        } else if let (Some(Val::Symbol(name)), val) = (args.get(0), args.get(1)) {
            if let Some(res) = self.eval(val, env) {
                self.env.definitions.borrow_mut().insert(name.clone(), res);
                Val::Nil
            } else {
                Val::RuntimeError(Error::Unknown)
            }
        } else {
            Val::RuntimeError(Error::Unknown)
        }
    }

    fn validate_lambda_args(args: &Val) -> bool {
        match args {
            Val::Symbol(_) => true,
            Val::List(list) => {
                let mut arg_set = HashSet::new();
                for arg in list.iter() {
                    if let Val::Symbol(sym) = arg {
                        if arg_set.contains(sym) {
                            return false;
                        } else {
                            arg_set.insert(sym.clone());
                        }
                    } else {
                        return false;
                    }
                }
                if arg_set.contains(&".".to_owned()) && list.len() > 1 {
                    if let Some(Val::Symbol(sym)) = list.get(list.len() - 2) {
                        if sym != "." {
                            return false;
                        }
                    } else {
                        return false;
                    }
                }
                return true;
            }
            _ => false,
        }
    }

    fn fill_lambda_args(args: &Val, vals: &[Val], env: &Env) -> Result<(), Error> {
        match args {
            Val::Symbol(sym) => {
                let mut vals_vec = Vec::new();
                vals_vec.resize(vals.len(), Val::Nil);
                let len = vals_vec.len();
                vals_vec[..len].clone_from_slice(vals);
                env.set_val(sym, Val::List(vals_vec));
                Ok(())
            }
            Val::List(list) => {
                let has_variadic = list.get(list.len() - 2) == Some(&Val::Symbol(".".to_owned()));
                let named_arg_length = if has_variadic {
                    list.len() - 2
                } else {
                    list.len()
                };
                if vals.len() < named_arg_length {
                    return Err(Error::IncorrectNumberOfArguments);
                }

                for (idx, arg) in list[..named_arg_length].iter().enumerate() {
                    if let Val::Symbol(sym) = arg {
                        if let Some(val) = vals.get(idx) {
                            env.set_val(sym, val.clone());
                        } else {
                            return Err(Error::IncorrectNumberOfArguments);
                        }
                    } else {
                        return Err(Error::InvalidArgumentList);
                    }
                }
                if has_variadic {
                    if let Some(Val::Symbol(sym)) = list.last() {
                        let mut variadic_vec = Vec::new();
                        variadic_vec.resize(vals.len() - named_arg_length, Val::Nil);
                        variadic_vec.clone_from_slice(&vals[named_arg_length..]);
                        env.set_val(sym, Val::List(variadic_vec));
                    } else {
                        return Err(Error::InvalidArgumentList);
                    }
                }
                return Ok(());
            }
            _ => Err(Error::InvalidArgumentList),
        }
    }

    fn lambda(&mut self, args: &[Val], env: &Rc<Box<Env>>) -> Val {
        if args.len() < 2 {
            Val::RuntimeError(Error::IncorrectNumberOfArguments)
        } else {
            if let (Some(lamdba_args_ref), body_ref) = (args.get(0), &args[1..]) {
                let lambda_args = lamdba_args_ref.clone();
                if !VM::validate_lambda_args(&lambda_args) {
                    return Val::RuntimeError(Error::InvalidArgumentList);
                }
                let mut body = Vec::new();
                body.resize(body_ref.len(), Val::Nil);
                let len = body.len();
                body[..(len)].clone_from_slice(body_ref);
                let lambda_env = Rc::new(Box::new(Env::new(Some(env.clone()))));
                Val::Proc(Procedure::new(
                    "lambda".to_owned(),
                    Box::new(move |vm: &mut VM, ast, _| {
                        if let Err(err) = VM::fill_lambda_args(&lambda_args, ast, &lambda_env) {
                            return Val::RuntimeError(err);
                        }
                        let mut ret = Val::Nil;
                        for list in body.iter() {
                            ret = vm.eval(Some(list), &lambda_env).unwrap_or(Val::Nil);
                        }
                        return ret;
                    }),
                ))
            } else {
                Val::RuntimeError(Error::SyntaxError(None))
            }
        }
    }

    fn add(&mut self, args: &[Val], env: &Rc<Box<Env>>) -> Val {
        if args.len() < 2 {
            return Val::RuntimeError(Error::IncorrectNumberOfArguments);
        }
        let evaluated: Vec<Val> = args
            .iter()
            .map(|arg| self.eval(Some(arg), env).unwrap())
            .collect();
        if let Some(Val::Int(_)) = evaluated.first() {
            VM::add_ints(&evaluated)
        } else if let Some(Val::Float(_)) = evaluated.first() {
            VM::add_floats(&evaluated)
        } else {
            Val::RuntimeError(Error::IncorrectType)
        }
    }
    fn add_ints(args: &[Val]) -> Val {
        let mut sum = 0i64;
        for val in args.iter() {
            if let Val::Int(i) = val {
                sum += i;
            } else {
                return Val::RuntimeError(Error::IncorrectType);
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
                return Val::RuntimeError(Error::IncorrectType);
            }
        }
        return Val::Float(sum);
    }
    fn sub(&mut self, args: &[Val], env: &Rc<Box<Env>>) -> Val {
        if args.len() < 2 {
            return Val::RuntimeError(Error::IncorrectNumberOfArguments);
        }
        let evaluated: Vec<Val> = args
            .iter()
            .map(|arg| self.eval(Some(arg), env).unwrap())
            .collect();
        if let Some(Val::Int(_)) = evaluated.first() {
            VM::sub_ints(&evaluated)
        } else if let Some(Val::Float(_)) = evaluated.first() {
            VM::sub_floats(&evaluated)
        } else {
            Val::RuntimeError(Error::IncorrectType)
        }
    }
    fn sub_ints(args: &[Val]) -> Val {
        let mut res = 0i64;
        for val in args.iter() {
            if let Val::Int(i) = val {
                res -= i;
            } else {
                return Val::RuntimeError(Error::IncorrectType);
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
                return Val::RuntimeError(Error::IncorrectType);
            }
        }
        return Val::Float(res);
    }
    fn mul(&mut self, args: &[Val], env: &Rc<Box<Env>>) -> Val {
        if args.len() < 2 {
            return Val::RuntimeError(Error::IncorrectNumberOfArguments);
        }
        let evaluated: Vec<Val> = args
            .iter()
            .map(|arg| self.eval(Some(arg), env).unwrap())
            .collect();
        if let Some(Val::Int(_)) = evaluated.first() {
            VM::mul_ints(&evaluated)
        } else if let Some(Val::Float(_)) = evaluated.first() {
            VM::mul_floats(&evaluated)
        } else {
            Val::RuntimeError(Error::IncorrectType)
        }
    }
    fn mul_ints(args: &[Val]) -> Val {
        let mut res = 1i64;
        for val in args.iter() {
            if let Val::Int(i) = val {
                res *= i;
            } else {
                return Val::RuntimeError(Error::IncorrectType);
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
                return Val::RuntimeError(Error::IncorrectType);
            }
        }
        return Val::Float(res);
    }
    fn div(&mut self, args: &[Val], env: &Rc<Box<Env>>) -> Val {
        if args.len() < 2 {
            return Val::RuntimeError(Error::IncorrectNumberOfArguments);
        }
        let evaluated: Vec<Val> = args
            .iter()
            .map(|arg| self.eval(Some(arg), env).unwrap())
            .collect();
        if let Some(Val::Int(_)) = evaluated.first() {
            VM::div_ints(&evaluated)
        } else if let Some(Val::Float(_)) = evaluated.first() {
            VM::div_floats(&evaluated)
        } else {
            Val::RuntimeError(Error::IncorrectType)
        }
    }
    fn div_ints(args: &[Val]) -> Val {
        if let Some(Val::Int(first)) = args.first() {
            let mut res = *first;
            for val in args[1..].iter() {
                if let Val::Int(i) = val {
                    res /= i;
                } else {
                    return Val::RuntimeError(Error::IncorrectType);
                }
            }
            return Val::Int(res);
        } else {
            Val::RuntimeError(Error::IncorrectType)
        }
    }
    fn div_floats(args: &[Val]) -> Val {
        if let Some(Val::Float(first)) = args.first() {
            let mut res = *first;
            for val in args[1..].iter() {
                if let Val::Float(i) = val {
                    res /= i;
                } else {
                    return Val::RuntimeError(Error::IncorrectType);
                }
            }
            return Val::Float(res);
        } else {
            Val::RuntimeError(Error::IncorrectType)
        }
    }
    fn search_env(&mut self, name: &String, env: &Env) -> Option<Val> {
        if let Some(val) = env.definitions.borrow().get(name) {
            Some(val.clone())
        } else if let Some(parent) = &env.parent {
            self.search_env(name, &parent)
        } else {
            None
        }
    }

    pub fn eval(&mut self, arg: Option<&Val>, env: &Rc<Box<Env>>) -> Option<Val> {
        if let Some(val) = arg {
            match val {
                Val::Symbol(name) => Some(
                    self.search_env(name, env)
                        .unwrap_or(Val::RuntimeError(Error::SymbolNotDefined(name.clone()))),
                ),
                Val::List(list) => {
                    if let Some(Val::Proc(procedure)) = self.eval(list.first(), env) {
                        Some(procedure.call(self, &list[1..], env.clone()))
                    } else {
                        Some(Val::RuntimeError(Error::IncorrectType))
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
                    .map(|ast| {
                        self.eval(Some(&Val::from(ast)), &self.env.clone())
                            .unwrap_or(Val::Nil)
                    })
                    .collect();
                return Ok(vals.last().unwrap_or(&Val::Nil).clone());
            }
            Err(e) => Err(e),
        }
    }
}
