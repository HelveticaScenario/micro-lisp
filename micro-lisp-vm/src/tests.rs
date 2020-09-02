#[cfg(test)]
mod tests {
    use crate::AST;
    use crate::AST::{List, Symbol};
    use crate::parse;
    use crate::Error;

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
        let expected_result = Ok(vec![List(vec![
            Symbol("hello".to_owned())
        ])]);
        assert_eq!(parse(code), expected_result);
    }

    #[test]
    fn syntax_error() {
        let code = "(hello))".to_owned();
        let expected_result = Err(Error::SyntaxError);
        assert_eq!(parse(code), expected_result);
    }

    #[test]
    fn parse_complex() {
        let code = r#"
            (foo bar  baz ( a 
                bcd e 123
                5.9
                "Whats up" \  "Doc"
            ) (poop))

            (pee peee)


            
        "#.to_owned();
        let expected_result = Ok(vec![
            List(vec![
                sym("foo"),
                sym("bar"),
                sym("baz"),
                List(vec![
                    sym("a"),
                    sym("bcd"),
                    sym("e"),
                    sym("123"),
                    sym("5.9"),
                    sym("\"Whats"),
                    sym("up\""),
                    sym(" "),
                    sym("\"Doc\"")
                ]),
                List(vec![
                    sym("poop")
                ])
            ]),
            List(vec![
                sym("pee"),
                sym("peee")
            ])
        ]);
        assert_eq!(parse(code), expected_result);
    }
}
