#[cfg(test)]
mod tests {
    use crate::Error;
    use crate::AST;
    use crate::AST::{Bool, Float, Int, List, Nil, Str, Symbol};
    use crate::VM;

    fn sym(s: &'static str) -> AST {
        Symbol(s.to_owned())
    }

    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }

    #[test]
    fn parse_simple() {
        let code = "(hello)".to_owned();
        let expected_result = Ok(vec![List(vec![Symbol("hello".to_owned())])]);
        assert_eq!(VM::parse(code), expected_result);
    }

    #[test]
    fn syntax_error() {
        let code = " (hello))".to_owned();
        let expected_result = Err(Error::SyntaxError(Some(8)));
        assert_eq!(VM::parse(code), expected_result);
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
            List(vec![
                sym("foo"),
                sym("bar"),
                sym("baz"),
                List(vec![
                    sym("a"),
                    sym("bcd"),
                    sym("e"),
                    Int(123),
                    Float(5.9),
                    Bool(true),
                    Str("Whats up".to_owned()),
                    Str("Doc".to_owned()),
                    Nil,
                ]),
                List(vec![sym("poop")]),
            ]),
            List(vec![sym("pee"), sym("peee")]),
        ]);
        assert_eq!(VM::parse(code), expected_result);
    }

    #[test]
    fn simple_exec() {
        let code = "(+ 1 1)".to_owned();
        let expected_result = Int(2);
        let vm = VM::new();
        assert_eq!(vm.borrow_mut().exec(code), expected_result);
    }
}
