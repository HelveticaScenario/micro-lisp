mod tests;

#[derive(Debug, Clone, PartialEq)]
pub enum AST {
    Symbol(String),
    List(Vec<AST>),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Error {
    SyntaxError,
}

pub fn parse(code: String) -> Result<Vec<AST>, Error> {
    let mut ast_stack: Vec<AST> = vec![AST::List(vec![])];
    let mut escaped: bool = false;
    for character in code.chars() {
        match (character, escaped) {
            ('(', false) => {
                // dont allow new list if currently in a symbol
                if let Some(AST::List(_)) = ast_stack.last() {
                    ast_stack.push(AST::List(vec![]));
                } else {
                    return Err(Error::SyntaxError);
                }
            }
            (')', false) => {
                if let Some(AST::Symbol(_)) = ast_stack.last() {
                    let sym = ast_stack.pop().unwrap();
                    if let Some(AST::List(list)) = ast_stack.last_mut() {
                        list.push(sym);
                    }
                }
                // there is an implicit root list, so if we try to close a list and all we have is the
                // root then there is a syntax error
                if ast_stack.len() == 1 {
                    return Err(Error::SyntaxError);
                }
                if let Some(AST::List(list)) = ast_stack.pop() {
                    if let Some(AST::List(container)) = ast_stack.last_mut() {
                        container.push(AST::List(list))
                    } else {
                        return Err(Error::SyntaxError);
                    }
                } else {
                    return Err(Error::SyntaxError);
                }
            }
            // ('"', false) => {
            //     if let Some(AST::LispString(_)) = ast_stack.last() {
            //         let s = ast_stack.pop().unwrap();
            //         if let Some(AST::List(list)) = ast_stack.last_mut() {
            //             list.push(s);
            //         } else {
            //             return Err(Error::SyntaxError);
            //         }
            //         continue;
            //     } else if let Some(AST::List(_)) = ast_stack.last() {
            //         ast_stack.push(AST::LispString("".to_owned()));
            //         continue;
            //     }

            //     return Err(Error::SyntaxError);
            // }
            ('\\', false) => {
                escaped = true;
            }
            (' ', false) | ('\n', false) | ('\t', false) | ('\r', false) => {
                if let Some(AST::Symbol(_)) = ast_stack.last() {
                    let sym = ast_stack.pop().unwrap();
                    if let Some(AST::List(list)) = ast_stack.last_mut() {
                        list.push(sym);
                    }
                }
            }
            _ => {
                if let Some(AST::Symbol(sym)) = ast_stack.last_mut() {
                    sym.push(character);
                } else if let Some(AST::List(_)) = ast_stack.last() {
                    ast_stack.push(AST::Symbol(String::from(character)))
                }
                // if let Some(curr) = &mut current {
                //     match curr {
                //         AST::List(_) => return err,
                //         AST::LispString(s) => {
                //             if c == '"' {
                //                 current = None
                //             }
                //         }
                //     }
                // } else if c == '\\' && !escaped {
                //     escaped = true
                // } else if c == '"' {
                //     current = Some(AST::Symbol(String::from(c)))
                // }
                escaped = false;
            }
        }
    }
    if ast_stack.len() != 1 {
        return Err(Error::SyntaxError);
    }
    if let Some(AST::List(root)) = ast_stack.pop() {
        return Ok(root);
    }
    return Err(Error::SyntaxError);
}

pub fn hello() {
    println!("hello lib");
}
