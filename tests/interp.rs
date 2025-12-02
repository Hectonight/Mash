use logos::Logos;
use mash::interp::interp_test;
use mash::lexer::{PeekableLexer, Token};
use mash::parser::parser;
use mash::type_check::typify;

fn runner(s: String) -> (String, String, i32) {
    let lexer = Token::lexer(&s);
    let mut peekable = PeekableLexer::new(lexer);
    let parsed = parser(&mut peekable);
    let typed = typify(parsed.unwrap()).unwrap();

    let mut out = vec![];
    let mut err = vec![];
    let code = interp_test(&typed, &mut out, &mut err).expect("Program Failed");
    let stdout = String::from_utf8(out).unwrap();
    let stderr = String::from_utf8(err).unwrap();
    (stdout, stderr, code)
}

fn stdout_compare(prog: &str, expected: &str) {
    let (stdout, stderr, code) = runner(prog.to_string());
    assert_eq!(code, 0);
    assert_eq!(stdout, expected.to_string());
    assert_eq!(stderr, "");
}

#[test]
fn literals_prog() {
    stdout_compare("print true;", "true\n");
    stdout_compare("print null;", "null\n");
    stdout_compare("print false;", "false\n");
    stdout_compare("print 1;", "1\n");
    stdout_compare("print 23;", "23\n");
    stdout_compare("print 2 + 1;", "3\n");
    stdout_compare("print -1;", "-1\n");
    stdout_compare("print 0b1001;", "9\n");
}

#[test]
fn let_prog() {
    stdout_compare("let a = 4; print a;", "4\n");
    stdout_compare("let a = null; print a;", "null\n");
    stdout_compare("let a = true;", "");
}

#[test]
fn assignments() {
    stdout_compare("let a = 4; a -= 3; print a;", "1\n");
    stdout_compare("let a = 4; a = 2; print a;", "2\n");
    stdout_compare("let a = 4; a += 1; print a;", "5\n");
    stdout_compare("let a = 4; a *= 5; print a;", "20\n");
}
