use logos::Logos;
use mash::compile::compile;
use mash::lexer::{PeekableLexer, Token};
use mash::parser::parser;
use mash::type_check::typify;
use std::path::Path;
use std::process::Command;

fn runner(s: String, file: &str) -> (String, String, i32) {
    let lexer = Token::lexer(&s);
    let mut peekable = PeekableLexer::new(lexer);
    let parsed = parser(&mut peekable);
    let typed = typify(parsed.unwrap()).unwrap();
    compile(typed, Path::new(&format!("/out/{}.s", file))).unwrap();

    Command::new("make")
                     .arg(format!("out/{}", file))
                     .output()
                     .expect("failed to execute make");
    let output = Command::new(format!("out/{}", file))
        .output()
        .expect("failed to execute make");

    (String::from_utf8(output.stdout).unwrap(),
     String::from_utf8(output.stderr).unwrap(),
     output.status.code().unwrap())

}


fn stdout_compare_file(prog: &str, expected: &str, file: &str) {
    let (stdout, stderr, code) = runner(prog.to_string(), file);
    assert_eq!(code, 0);
    assert_eq!(stdout, expected.to_string());
    assert_eq!(stderr, "");
}

fn get_stdout_compare(file: &str) -> impl Fn(&str, &str) + '_ {
    |a, b| stdout_compare_file(a, b, file)
}

#[test]
fn literals_prog() {
    let stdout_compare = get_stdout_compare("literals");
    stdout_compare("print true;", "true\n");
    stdout_compare("print null;", "null\n");
    stdout_compare("print false;", "false\n");
    stdout_compare("print 1;", "1\n");
    stdout_compare("print 23;", "23\n");
    stdout_compare("print 2 + 1;", "3\n");
    stdout_compare("print -1;", "-1\n");
}


#[test]
fn let_prog() {
    let stdout_compare = get_stdout_compare("let");
    stdout_compare("let a = 4; print a;", "4\n");
    stdout_compare("let a = null; print a;", "null\n");
    stdout_compare("let a = true;", "");
}


