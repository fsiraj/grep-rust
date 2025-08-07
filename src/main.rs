use std::env;
use std::io;
use std::process;

#[derive(Debug, PartialEq)]
enum Line {
    Start,
    End,
}

#[derive(Debug, PartialEq)]
enum Escape {
    Digit,
    Alphaneumeric,
}

#[derive(Debug, PartialEq)]
enum Quantity {
    OneOrMore,
    ZeroOrMore,
    ZeroOrOne,
}

#[derive(Debug, PartialEq)]
enum Pattern {
    Char(char),
    Class(Escape),
    Bound(Line),
    Group {
        group: Regex,
        negate: bool,
    },
    Alternates {
        alternates: Alternates,
    },
    Quantifier {
        pattern: Box<Pattern>,
        kind: Quantity,
    },
}

type Regex = Vec<Pattern>;
type Alternates = Vec<Regex>;

fn compile_regex(regex: &str) -> Regex {
    let mut iter = regex.char_indices();
    let mut patterns = Vec::new();

    while let Some((i, c)) = iter.next() {
        match c {
            '\\' => match iter.next() {
                Some((_, 'd')) => {
                    patterns.push(Pattern::Class(Escape::Digit));
                }
                Some((_, 'w')) => {
                    patterns.push(Pattern::Class(Escape::Alphaneumeric));
                }
                _ => {}
            },
            '[' => {
                let mut subpattern = String::new();
                loop {
                    match iter.next() {
                        Some((_, ']')) => break,
                        Some((_, other)) => subpattern.push(other),
                        None => panic!("unclosed [ character group"),
                    }
                }
                if !subpattern.is_empty() {
                    let (start, negate) =
                        if subpattern.len() > 1 && subpattern.chars().next().unwrap() == '^' {
                            (1, true)
                        } else {
                            (0, false)
                        };
                    let group = compile_regex(&subpattern[start..]);
                    patterns.push(Pattern::Group {
                        group: group,
                        negate: negate,
                    });
                }
            }
            '(' => {
                let mut depth = 0;
                let mut subpattern = String::new();
                let mut alternates = Vec::new();
                loop {
                    match iter.next() {
                        Some((_, ')')) => {
                            if depth == 0 {
                                alternates.push(subpattern.clone());
                                subpattern.clear();
                                break;
                            }
                            depth -= 1;
                            subpattern.push(')');
                        }
                        Some((_, '(')) => {
                            depth += 1;
                            subpattern.push('(');
                        }
                        Some((_, '|')) => {
                            if depth == 0 {
                                alternates.push(subpattern.clone());
                                subpattern.clear();
                                continue;
                            }
                            subpattern.push('|');
                        }
                        Some((_, other)) => subpattern.push(other),
                        None => panic!("uncloded ( alternates group"),
                    }
                }
                if !alternates.is_empty() {
                    let alternates = alternates.iter().map(|r| compile_regex(r)).collect();
                    patterns.push(Pattern::Alternates { alternates });
                }
            }
            '^' if i == 0 => {
                patterns.push(Pattern::Bound(Line::Start));
            }
            '$' if i == regex.len() - 1 => {
                patterns.push(Pattern::Bound(Line::End));
            }
            '+' | '*' | '?' => {
                let Some(pattern) = patterns.pop() else {
                    panic!("quantifier missing pattern");
                };
                patterns.push(Pattern::Quantifier {
                    pattern: Box::new(pattern),
                    kind: match c {
                        '+' => Quantity::OneOrMore,
                        '*' => Quantity::ZeroOrMore,
                        '?' => Quantity::ZeroOrOne,
                        _ => unreachable!(),
                    },
                });
            }
            _ => {
                patterns.push(Pattern::Char(c));
            }
        }
    }
    patterns
}

fn match_quantifier(
    text: &[char],
    pattern: &Pattern,
    kind: &Quantity,
    next_patterns: &[Pattern],
) -> Option<usize> {
    // try match once, if no match, all three cases return, else ? returns and other two continue
    let mut size = 0;
    match match_substr(text, std::slice::from_ref(pattern)) {
        Some(next_size) => match kind {
            Quantity::ZeroOrOne => return Some(next_size),
            _ => size += next_size,
        },
        None => match kind {
            Quantity::OneOrMore => return None,
            _ => return Some(0),
        },
    }
    // now keep looping for * and + until rest of pattern matches rest of string
    loop {
        if let Some(_) = match_substr(&text[size..], next_patterns) {
            return Some(size);
        }
        if let Some(next_size) = match_substr(&text[size..], std::slice::from_ref(pattern)) {
            size += next_size;
        } else {
            return Some(size);
        }
    }
}

// returns Some(size) of match, None if no match
fn match_substr(text: &[char], patterns: &[Pattern]) -> Option<usize> {
    eprintln!("{:?} - {:?}", text, patterns);

    if patterns.is_empty() {
        // pattern ended, nothing more to check
        return Some(0);
    }
    if text.is_empty() {
        if let Pattern::Bound(Line::End) = patterns[0] {
            // end of string matches $
            return Some(0);
        }
        if let Pattern::Quantifier { pattern: _, kind } = &patterns[0] {
            if matches!(kind, Quantity::ZeroOrMore | Quantity::ZeroOrOne) {
                return Some(0);
            }
        }
        // we still have patterns (not $, *, or ?) but text ended
        return None;
    }

    let next_p = &patterns[0];
    let next_c = text[0];

    let result = match next_p {
        Pattern::Char(pattern_c) => (next_c == *pattern_c || *pattern_c == '.').then_some(1),
        Pattern::Class(c_class) => match c_class {
            Escape::Digit => next_c.is_numeric().then_some(1),
            Escape::Alphaneumeric => (next_c.is_alphanumeric() || next_c == '_').then_some(1),
        },
        Pattern::Bound(kind) => match kind {
            Line::End => None,
            Line::Start => unreachable!(),
        },
        Pattern::Group { group, negate } => match *negate {
            true => group
                .iter()
                .all(|p| match_substr(text, std::slice::from_ref(p)).is_none())
                .then_some(1),
            false => group
                .iter()
                .any(|p| match_substr(text, std::slice::from_ref(p)).is_some())
                .then_some(1),
        },
        Pattern::Alternates { alternates } => alternates.iter().find_map(|r| match_substr(text, r)),
        Pattern::Quantifier { pattern, kind } => {
            match_quantifier(text, pattern, kind, &patterns[1..])
        }
    };
    let Some(size) = result else {
        return None;
    };

    match match_substr(&text[size..], &patterns[1..]) {
        Some(next_size) => Some(size + next_size),
        None => None,
    }
}

fn match_pattern(text: &str, patterns: &str) -> Option<usize> {
    let text = text.chars().collect::<Vec<char>>();
    let regex = compile_regex(patterns);
    eprintln!("{:?} ({:?})", text, text.len());
    eprintln!("{:?}", regex);

    if let Pattern::Bound(Line::Start) = regex[0] {
        // no need to test any other starting positions
        return match_substr(text.as_slice(), &regex[1..]);
    }
    // test all starting positions
    for start in 0..text.len() {
        if let Some(size) = match_substr(&text[start..], &regex) {
            return Some(size);
        }
    }
    // tried all starting positions
    None
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

    if let Some(size) = match_pattern(&input_line, &pattern) {
        eprintln!("Match! ({} chars of {})", size, input_line.len());
        process::exit(0)
    } else {
        eprintln!("No match");
        process::exit(1)
    }
}
