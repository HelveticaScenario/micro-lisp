use crate::VM;
use rand::Rng;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

pub struct Env {
  pub parent: Option<Rc<Box<Env>>>,
  pub definitions: RefCell<HashMap<String, Val>>,
}

impl Env {
  pub fn new(parent: Option<Rc<Box<Env>>>) -> Self {
    Env {
      parent,
      definitions: RefCell::new(HashMap::new()),
    }
  }

  pub fn get_val(&self, key: &String) -> Option<Val> {
    if let Some(val) = self.definitions.borrow().get(key) {
      return Some(val.clone());
    } else if let Some(parent) = &self.parent {
      return parent.get_val(key);
    } else {
      return None;
    }
  }
  pub fn set_val(&self, key: &String, val: Val) -> Option<Val> {
    self.definitions.borrow_mut().insert(key.clone(), val)
  }

  pub fn is_root(&self) -> bool {
    self.parent.is_none()
  }
}

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

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
  SyntaxError(Option<usize>),
  DefineNotAtRoot,
  IncorrectNumberOfArguments,
  InvalidArgumentList,
  IncorrectType,
  SymbolNotDefined(String),
  Unknown,
}

#[derive(Clone)]
pub struct Procedure {
  _id: u128,
  name: String,
  func: Rc<Box<dyn Fn(&mut VM, &[Val], Rc<Box<Env>>) -> Val>>,
}

impl Procedure {
  pub fn new(name: String, func: Box<dyn Fn(&mut VM, &[Val], Rc<Box<Env>>) -> Val>) -> Self {
    Self {
      _id: rand::thread_rng().gen(),
      name,
      func: Rc::new(func),
    }
  }
  pub fn call(&self, vm: &mut VM, args: &[Val], env: Rc<Box<Env>>) -> Val {
    self.func.clone()(vm, args, env)
  }
}

impl PartialEq for Procedure {
  fn eq(&self, other: &Self) -> bool {
    self._id == other._id
  }
}

impl fmt::Debug for Procedure {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_struct("Procedure")
      .field("name", &self.name)
      .field("id", &self._id)
      .finish()
  }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Val {
  Symbol(String),
  Str(String),
  Int(i64),
  Float(f64),
  Bool(bool),
  List(Vec<Val>),
  Nil,
  Proc(Procedure),
  RuntimeError(Error), // Func(fn(Vec<Val>) -> Val),
}

impl From<&AST> for Val {
  fn from(ast: &AST) -> Self {
    match ast {
      AST::Symbol(s) => Val::Symbol(s.clone()),
      AST::Str(s) => Val::Str(s.clone()),
      AST::Int(i) => Val::Int(*i),
      AST::Float(f) => Val::Float(*f),
      AST::Bool(b) => Val::Bool(*b),
      AST::List(l) => Val::List(l.iter().map(|ast| Val::from(ast)).collect()),
      AST::Nil => Val::Nil,
    }
  }
}
