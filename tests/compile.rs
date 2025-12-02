// Combined full test suite for mash language

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
        .expect("failed to execute executable");

    (
        String::from_utf8(output.stdout).unwrap(),
        String::from_utf8(output.stderr).unwrap(),
        output.status.code().unwrap(),
    )
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

#[test]
fn assignments() {
    let stdout_compare = get_stdout_compare("assignments");
    stdout_compare("let a = 4; a -= 3; print a;", "1\n");
    stdout_compare("let a = 4; a = 2; print a;", "2\n");
    stdout_compare("let a = 4; a += 1; print a;", "5\n");
    stdout_compare("let a = 4; a *= 5; print a;", "20\n");
}

#[test]
fn bitops_prog() {
    let stdout_compare = get_stdout_compare("bitops");
    stdout_compare(
        "let a = 6; let b = 3; print a & b; print a | b; print a ^ b; print ~a;",
        "2\n7\n5\n-7\n",
    );
}

#[test]
fn shifts_prog() {
    let stdout_compare = get_stdout_compare("shifts");
    stdout_compare(
        "let x = 1; let y = 5; print x << y; print 32 >> 3;",
        "32\n4\n",
    );
}

#[test]
fn precedence_prog() {
    let stdout_compare = get_stdout_compare("precedence");
    stdout_compare("print 1 + 2 * 3; print (1 + 2) * 3;", "7\n9\n");
}

#[test]
fn prefix_prog() {
    let stdout_compare = get_stdout_compare("prefix");
    stdout_compare(
        "print - -1; print +5; print !true; print ~0;",
        "1\n5\nfalse\n-1\n",
    );
}

#[test]
fn ifs_prog() {
    let stdout_compare = get_stdout_compare("ifs");
    stdout_compare(
        "let n = 75; if n >= 90 { print 4; } else if n >= 80 { print 3; } else { print 1; } print n;",
        "1\n75\n",
    );
}

#[test]
fn factorial_prog() {
    let stdout_compare = get_stdout_compare("fact");
    stdout_compare(
        "let n = 5; let result = 1; while n > 1 { result *= n; n -= 1; } print result;",
        "120\n",
    );
}

#[test]
fn fibonacci_prog() {
    let stdout_compare = get_stdout_compare("fib");
    stdout_compare(
        "let a = 0; let b = 1; let i = 0; let tmp = 0; while i < 6 { print a; tmp = a + b; a = b; b = tmp; i += 1; }",
        "0\n1\n1\n2\n3\n5\n",
    );
}

#[test]
fn bitflags_prog() {
    let stdout_compare = get_stdout_compare("bitflags");
    stdout_compare(
        "let flags = 0; let READ = 1; let WRITE = 2; let EXEC = 4; flags |= READ | WRITE; flags &= ~WRITE; let canRead = (flags & READ) != 0; let canWrite = (flags & WRITE) != 0; print canRead; print canWrite;",
        "true\nfalse\n",
    );
}

#[test]
fn assign_ops_prog() {
    let stdout_compare = get_stdout_compare("assign_ops");
    stdout_compare(
        "let a = 3; a += 4; a -= 1; a *= 2; a |= 8; a ^= 1; a &= 7; a <<= 2; a >>= 1; print a;",
        "10\n",
    );
}

#[test]
fn null_prog() {
    let stdout_compare = get_stdout_compare("null");
    stdout_compare(
        "let maybe = null; if maybe == null { print 0; } else { print 1; }",
        "0\n",
    );
}

#[test]
fn complex_expr_prog() {
    let stdout_compare = get_stdout_compare("complex");
    stdout_compare(
        "let x = 5; let y = 3; let z = x * y + (x << 2) - (~y); print z;",
        "39\n",
    );
}

#[test]
fn logic_prog() {
    let stdout_compare = get_stdout_compare("logic");
    stdout_compare(
        "print true || false; print false && true; print (1 < 2) && (2 < 3); print (1 > 2) || (2 > 1);",
        "true\nfalse\ntrue\ntrue\n",
    );
}

#[test]
fn bases_and_null_prog() {
    let stdout_compare = get_stdout_compare("bases_and_null");
    stdout_compare("print 0b1010;", "10\n");
    stdout_compare("print 0B1010;", "10\n");
    stdout_compare("print 0o12;", "10\n");
    stdout_compare("print 0O12;", "10\n");
    stdout_compare("print 0xA;", "10\n");
    stdout_compare("print 0XA;", "10\n");
    stdout_compare("print 0xff;", "255\n");
    stdout_compare("print null;", "null\n");
}

#[test]
fn chars_prog() {
    let stdout_compare = get_stdout_compare("chars");
    stdout_compare(r"print 'a';", "a\n");
    stdout_compare(r"print '\n';", "\n\n");
    stdout_compare(r"print '\r';", "\r\n");
    stdout_compare(r"print '\\';", "\\\n");
    stdout_compare(r"print '\'';", "'\n");
    stdout_compare(r"print '\0';", "\n");
}

#[test]
fn combined_complex_prog() {
    let stdout_compare = get_stdout_compare("combined_complex");
    stdout_compare(
        "let a = 0b11; let b = 0x2; let c = 0o4; let flags = 0; flags |= a | b; flags <<= 1; let s = flags & c; print a; print b; print c; print flags; print s;",
        "3\n2\n4\n6\n4\n",
    );
}
