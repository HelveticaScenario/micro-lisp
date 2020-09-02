use rand::Rng;
use std::fmt;
use std::rc::Rc;

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

#[derive(Clone)]
pub struct Procedure {
  _id: u128,
  name: String,
  func: Rc<Box<dyn Fn(&[Val]) -> Val>>,
}

impl Procedure {
  pub fn new(name: String, func: Box<dyn Fn(&[Val]) -> Val>) -> Self {
    Self {
      _id: rand::thread_rng().gen(),
      name,
      func: Rc::new(func),
    }
  }
  pub fn call(&self, args: &[Val]) -> Val {
    self.func.clone()(args)
  }
}

impl PartialEq for Procedure {
  fn eq(&self, other: &Self) -> bool {
    self._id == self._id
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
  RuntimeError, // Func(fn(Vec<Val>) -> Val),
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
