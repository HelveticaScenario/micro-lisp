#[cfg(test)]
mod tests {
    use crate::{Error, Val, AST, VM};

    fn sym(s: &'static str) -> AST {
        AST::Symbol(s.to_owned())
    }

    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }

    #[test]
    fn parse_simple() {
        let code = "(hello)".to_owned();
        let expected_result = Ok(vec![AST::List(vec![AST::Symbol("hello".to_owned())])]);
        assert_eq!(VM::parse(&code), expected_result);
    }

    #[test]
    fn syntax_error() {
        let code = " (hello))".to_owned();
        let expected_result = Err(Error::SyntaxError(Some(8)));
        assert_eq!(VM::parse(&code), expected_result);
    }

    #[test]
    fn parse_complex() {
        let code = r#"
            (foo bar  baz ( a 
                bcd e 123
                5.9 true
                "Whats up"  "Doc"
                nil
            ) (poop))

            (pee peee)


            
        "#
        .to_owned();
        let expected_result = Ok(vec![
            AST::List(vec![
                sym("foo"),
                sym("bar"),
                sym("baz"),
                AST::List(vec![
                    sym("a"),
                    sym("bcd"),
                    sym("e"),
                    AST::Int(123),
                    AST::Float(5.9),
                    AST::Bool(true),
                    AST::Str("Whats up".to_owned()),
                    AST::Str("Doc".to_owned()),
                    AST::Nil,
                ]),
                AST::List(vec![sym("poop")]),
            ]),
            AST::List(vec![sym("pee"), sym("peee")]),
        ]);
        assert_eq!(VM::parse(&code), expected_result);
    }

    #[test]
    fn simple_exec() {
        let code = "(define pi 3.14) (+ 1 1) (* 2.0 pi)".to_owned();
        let expected_result: Result<Val, Error> = Ok(Val::List(vec![Val::Nil, Val::Int(2), Val::Float(6.28)]));
        let vm = VM::new();
        assert_eq!(vm.borrow_mut().exec(&code), expected_result);
    }
}
