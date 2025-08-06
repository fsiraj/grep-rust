use std::env;
use std::io;
use std::process;

#[derive(Debug, PartialEq)]
enum MetaChar {
    LineStart,
    LineEnd,
}
#[derive(Debug, PartialEq)]
enum CharClass {
    Digit,
    Alphaneumeric,
}

#[derive(Debug, PartialEq)]
enum RegexToken {
    Char(char),
    Meta(MetaChar),
    Class(CharClass),
    Group {
        group: Vec<RegexToken>,
        negate: bool,
    },
}

fn compile_pattern(pattern: &str) -> Vec<RegexToken> {
    let mut iter = pattern.char_indices();
    let mut tokens = Vec::new();

    while let Some((_, c)) = iter.next() {
        match c {
            '\\' => match iter.next() {
                Some((_, 'd')) => {
                    tokens.push(RegexToken::Class(CharClass::Digit));
                }
                Some((_, 'w')) => {
                    tokens.push(RegexToken::Class(CharClass::Alphaneumeric));
                }
                _ => {}
            },
            '[' => {
                let mut sub_pattern = String::new();
                loop {
                    match iter.next() {
                        Some((_, ']')) => break,
                        Some((_, c_)) => sub_pattern.push(c_),
                        None => eprintln!("invalid character group"),
                    }
                }
                if sub_pattern.len() > 0 {
                    let (start, negate) =
                        if sub_pattern.len() > 1 && sub_pattern.chars().next().unwrap() == '^' {
                            (1, true)
                        } else {
                            (0, false)
                        };
                    let group = compile_pattern(&sub_pattern[start..]);
                    tokens.push(RegexToken::Group {
                        group: group,
                        negate: negate,
                    });
                }
            }
            '^' => {
                tokens.push(RegexToken::Meta(MetaChar::LineStart));
            }
            '$' => {
                tokens.push(RegexToken::Meta(MetaChar::LineEnd));
            }
            _ => {
                tokens.push(RegexToken::Char(c));
            }
        }
    }
    tokens
}

fn match_token(c: char, token: &RegexToken) -> bool {
    match token {
        RegexToken::Char(c_) => c == *c_,
        RegexToken::Class(c_class) => match c_class {
            CharClass::Digit => c.is_numeric(),
            CharClass::Alphaneumeric => c.is_alphanumeric() || c == '_',
        },
        RegexToken::Group { group, negate } => {
            if *negate {
                group.iter().all(|t| !match_token(c, t))
            } else {
                group.iter().any(|t| match_token(c, t))
            }
        }
        RegexToken::Meta(_) => unreachable!(),
    }
}

fn match_substr(text: &str, pattern: &[RegexToken]) -> bool {
    // eprintln!("{:?} - {:?}", text, pattern);
    if pattern.len() == 0 {
        return true;
    }
    if pattern[0] == RegexToken::Meta(MetaChar::LineEnd) && pattern.len() == 1 {
        return text.len() == 0;
    }
    if text.len() == 0 {
        return false;
    }
    let c = text.chars().next().unwrap();
    if match_token(c, &pattern[0]) {
        // eprintln!("match, incrementing...");
        let next = c.len_utf8();
        return match_substr(&text[next..], &pattern[1..]);
    }
    false
}

fn match_pattern(text: &str, pattern: &str) -> bool {
    // eprintln!("{:?} - {:?}", text, pattern);
    let regex_tokens = compile_pattern(pattern);
    // eprintln!("{:?}", regex_tokens);

    if regex_tokens.get(0) == Some(&RegexToken::Meta(MetaChar::LineStart)) {
        return match_substr(text, &regex_tokens[1..]);
    }

    for (i, _) in text.char_indices() {
        if match_substr(&text[i..], &regex_tokens) {
            return true;
        }
    }
    false
}

// Usage: echo <input_text> | your_program.sh -E <pattern>
fn main() {
    if env::args().nth(1).unwrap() != "-E" {
        println!("Expected first argument to be '-E'");
        process::exit(1);
    }

    let pattern = env::args().nth(2).unwrap();
    let mut input_line = String::new();

    io::stdin().read_line(&mut input_line).unwrap();

    if match_pattern(&input_line, &pattern) {
        eprintln!("Match");
        process::exit(0)
    } else {
        eprintln!("No match");
        process::exit(1)
    }
}
