use clap::Parser;
use std::fs;
use std::io;
use std::path;
use std::process;
use walkdir::WalkDir;

// Core matching logic

#[derive(Debug, PartialEq)]
enum Meta {
    Digit,
    Alphanumeric,
    Start,
    End,
    OneOrMore,
    ZeroOrMore,
    ZeroOrOne,
}

#[derive(Debug, PartialEq)]
enum Pattern {
    Char(char),
    Class(Meta),
    Bound(Meta),
    CharGroup { group: Regex, negate: bool },
    CaptureGroup { regexes: Alternates },
    Quantifier { pattern: Box<Pattern>, kind: Meta },
    BackRef(usize),
}

type Regex = Vec<Pattern>;
type Alternates = Vec<Regex>;

// Returns an AST form of the regex
fn compile_regex(regex: &str) -> Regex {
    let mut iter = regex.char_indices();
    let mut patterns = Vec::new();

    while let Some((i, c)) = iter.next() {
        match c {
            '\\' => match iter.next() {
                Some((_, 'd')) => patterns.push(Pattern::Class(Meta::Digit)),
                Some((_, 'w')) => patterns.push(Pattern::Class(Meta::Alphanumeric)),
                Some((_, other)) if other.is_numeric() => {
                    let index = other.to_digit(10).unwrap() as usize;
                    if index == 0 {
                        panic!("backref are 1-indexed, upto 9")
                    }
                    patterns.push(Pattern::BackRef(index))
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
                    let (start, negate) = if subpattern.starts_with('^') {
                        (1, true)
                    } else {
                        (0, false)
                    };
                    let group = compile_regex(&subpattern[start..]);
                    patterns.push(Pattern::CharGroup { group, negate });
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
                                alternates.push(subpattern);
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
                                alternates.push(subpattern);
                                subpattern = String::new();
                                continue;
                            }
                            subpattern.push('|');
                        }
                        Some((_, other)) => subpattern.push(other),
                        None => panic!("unclosed ( alternates group"),
                    }
                }
                if !alternates.is_empty() {
                    let alternates = alternates.iter().map(|r| compile_regex(r)).collect();
                    patterns.push(Pattern::CaptureGroup {
                        regexes: alternates,
                    });
                }
            }
            '^' if i == 0 => {
                patterns.push(Pattern::Bound(Meta::Start));
            }
            '$' if i == regex.len() - 1 => {
                patterns.push(Pattern::Bound(Meta::End));
            }
            '+' | '*' | '?' => {
                let Some(pattern) = patterns.pop() else {
                    panic!("quantifier missing pattern");
                };
                patterns.push(Pattern::Quantifier {
                    pattern: Box::new(pattern),
                    kind: match c {
                        '+' => Meta::OneOrMore,
                        '*' => Meta::ZeroOrMore,
                        '?' => Meta::ZeroOrOne,
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

fn match_char(next_c: &char, pattern_c: &char) -> Option<usize> {
    (*next_c == *pattern_c || *pattern_c == '.').then_some(1)
}

fn match_digit(next_c: &char) -> Option<usize> {
    next_c.is_numeric().then_some(1)
}

fn match_alphanumeric(next_c: &char) -> Option<usize> {
    (next_c.is_alphanumeric() || *next_c == '_').then_some(1)
}

fn match_group(text: &[char], group: &Vec<Pattern>, negate: &bool) -> Option<usize> {
    match *negate {
        true => group
            .iter()
            .all(|p| match_substr(text, std::slice::from_ref(p)).is_none())
            .then_some(1),
        false => group
            .iter()
            .any(|p| match_substr(text, std::slice::from_ref(p)).is_some())
            .then_some(1),
    }
}

fn match_quantifier(
    text: &[char],
    pattern: &Pattern,
    kind: &Meta,
    next_patterns: &[Pattern],
) -> Option<usize> {
    let pattern = std::slice::from_ref(pattern);
    // at least one match for +
    let mut size = match kind {
        Meta::OneOrMore => match_substr(text, pattern)?,
        _ => 0,
    };
    loop {
        // try 0 matches for + and *, >1 for +
        if text[size..].is_empty() && match_substr(&text[size..], next_patterns).is_some() {
            return Some(size);
        }
        if !next_patterns.is_empty() && match_substr(&text[size..], next_patterns).is_some() {
            return Some(size);
        }
        match kind {
            Meta::ZeroOrOne => {
                // don't loop for ?
                return match_substr(&text[size..], pattern);
            }
            _ => {
                // keep moving forward for + and *
                if let Some(next_size) = match_substr(&text[size..], pattern) {
                    size += next_size;
                } else {
                    return None;
                }
            }
        }
    }
}

fn match_alternates(text: &[char], alternates: &Vec<Vec<Pattern>>) -> Option<usize> {
    alternates.iter().find_map(|r| match_substr(text, r))
}

// returns Some(size) of match, None if no match
fn match_substr(text: &[char], patterns: &[Pattern]) -> Option<usize> {
    // eprintln!(
    //     "{:?} - {:?} ({:?})",
    //     text.iter().collect::<String>(),
    //     patterns.get(0).unwrap_or(&Pattern::Char('~')),
    //     patterns.len()
    // );

    if patterns.is_empty() {
        return Some(0);
    }
    if text.is_empty() {
        if matches!(patterns[0], Pattern::Bound(Meta::End)) {
            return Some(0);
        }
        if let Pattern::Quantifier { pattern: _, kind } = &patterns[0] {
            if matches!(kind, Meta::ZeroOrMore | Meta::ZeroOrOne) {
                return Some(0);
            }
        }
        return None;
    }

    let next_p = &patterns[0];
    let next_c = &text[0];
    let result = match next_p {
        Pattern::Char(pattern_c) => match_char(next_c, pattern_c),
        Pattern::Class(c_class) => match c_class {
            Meta::Digit => match_digit(next_c),
            Meta::Alphanumeric => match_alphanumeric(next_c),
            _ => unreachable!(),
        },
        Pattern::Bound(kind) => match kind {
            Meta::End => None,
            _ => unreachable!(),
        },
        Pattern::CharGroup { group, negate } => match_group(text, group, negate),
        Pattern::CaptureGroup {
            regexes: alternates,
        } => match_alternates(text, alternates),
        Pattern::Quantifier { pattern, kind } => {
            match_quantifier(text, pattern, kind, &patterns[1..])
        }
        Pattern::BackRef(_index) => unimplemented!(),
    };
    let size = result?;
    Some(size + match_substr(&text[size..], &patterns[1..])?)
}

fn match_pattern(text: &str, regex: &str) -> Option<usize> {
    // eprintln!("{:?} ({:?})", text, text.len());
    // eprintln!("{:?} ({:?})", regex, regex.len());
    let text = text.chars().collect::<Vec<char>>();
    let regex = compile_regex(regex);
    // eprintln!("\n{:?} ({:?})", regex, regex.len());

    if matches!(regex[0], Pattern::Bound(Meta::Start)) {
        // no need to test any other starting positions
        return match_substr(text.as_slice(), &regex[1..]);
    }
    // test all starting positions
    (0..text.len()).find_map(|start| match_substr(&text[start..], &regex))
}

// Helpers

fn find_files_recursive(root: &str) -> Vec<String> {
    WalkDir::new(root)
        .into_iter()
        .filter_map(Result::ok)
        .filter(|entry| entry.file_type().is_file())
        .map(|entry| entry.path().display().to_string())
        .collect()
}

fn print_match(input: &str, fp: Option<&str>, show_prefix: bool) {
    if show_prefix {
        print!("{}:", fp.unwrap());
    }
    println!("{}", input);
}

// Argument Parsing

#[derive(Parser, Debug)]
struct Args {
    #[arg(short = 'E')]
    expression: String,

    #[arg(short='r', default_value_t=false, action=clap::ArgAction::SetTrue)]
    recursive: bool,

    paths: Vec<String>,
}

// Main

fn main() {
    let args = Args::parse();
    if args.recursive {
        if args.paths.len() != 1 || !path::Path::new(&args.paths[0]).is_dir() {
            eprintln!("Error: -r requires exactyly one path to a directory.");
            process::exit(1);
        };
    }

    let mut found = false;
    match !args.paths.is_empty() {
        // filepaths or directory
        true => {
            let filepaths = if args.recursive {
                find_files_recursive(&args.paths[0])
            } else {
                args.paths
            };
            let show_prefix = filepaths.len() > 1 || args.recursive;
            for fp in &filepaths {
                // eprintln!("Processing {:?}...", fp);
                fs::read_to_string(fp)
                    .expect("unable to open file")
                    .lines()
                    .filter(|input| match_pattern(input, &args.expression).is_some())
                    .for_each(|input| {
                        print_match(&input, Some(fp), show_prefix);
                        found = true;
                    });
            }
        }
        // stdin
        false => {
            // simple stdin case
            let mut input = String::new();
            io::stdin().read_line(&mut input).unwrap();
            if match_pattern(&input, &args.expression).is_some() {
                print_match(&input, None, false);
                found = true;
            }
        }
    }

    if found {
        process::exit(0)
    } else {
        process::exit(1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn matches(text: &str, regex: &str) -> bool {
        match_pattern(text, regex).is_some()
    }

    #[test]
    fn test_literal_character() {
        assert!(matches("dog", "d"));
        assert!(!matches("dog", "f"));
    }

    #[test]
    fn test_digits() {
        assert!(matches("123", r"\d"));
        assert!(!matches("apple", r"\d"));
    }

    #[test]
    fn test_alphanumeric() {
        assert!(matches("pear", r"\w"));
        assert!(matches("STRAWBERRY", r"\w"));
        assert!(matches("129", r"\w"));
        assert!(matches("+#-_-%÷", r"\w"));
        assert!(!matches("%#÷=×+", r"\w"));
    }

    #[test]
    fn test_positive_character_groups() {
        assert!(matches("l", "[apple]"));
        assert!(!matches("banana", "[cdefgh]"));
        assert!(!matches("[]", "[pineapple]"));
    }

    #[test]
    fn test_negative_character_groups() {
        assert!(matches("apple", "[^xyz]"));
        assert!(matches("apple", "[^abc]"));
        assert!(!matches("banana", "[^anb]"));
        assert!(matches("orange", "[^opq]"));
    }

    #[test]
    fn test_combining_character_classes() {
        assert!(matches("sally has 3 apples", r"\d apple"));
        assert!(!matches("sally has 1 orange", r"\d apple"));
        assert!(matches("sally has 124 apples", r"\d\d\d apples"));
        assert!(!matches("sally has 12 apples", r"\d\\d\\d apples"));
        assert!(matches("sally has 3 dogs", r"\d \w\w\ws"));
        assert!(matches("sally has 4 dogs", r"\d \w\w\ws"));
        assert!(!matches("sally has 1 dog", r"\d \w\w\ws"));
    }

    #[test]
    fn test_start_of_string_anchor() {
        assert!(matches("log", "^log"));
        assert!(!matches("slog", "^log"));
    }

    #[test]
    fn test_end_of_string_anchor() {
        assert!(matches("cat", "cat$"));
        assert!(!matches("cats", "cat$"));
    }

    #[test]
    fn test_one_or_more_times() {
        assert!(matches("cat", "ca+t"));
        assert!(matches("caaats", "ca+at"));
        assert!(matches("a", "a+"));
        assert!(matches("aa", "a+a"));
        assert!(matches("aa", "aa+"));
        assert!(!matches("act", "ca+t"));
        assert!(!matches("ca", "ca+t"));
    }

    #[test]
    fn test_zero_or_more_times() {
        assert!(matches("cat", "ca*t"));
        assert!(matches("caaats", "ca*t"));
        assert!(matches("a", "a*"));
        assert!(matches("aa", "a*a"));
        assert!(matches("aa", "aa*"));
        assert!(!matches("act", "ca+t"));
        assert!(!matches("ca", "ca+t"));
    }

    #[test]
    fn test_zero_or_one_times() {
        assert!(matches("cat", "ca?t"));
        assert!(matches("act", "ca?t"));
        assert!(!matches("dog", "ca?t"));
        assert!(!matches("cag", "ca?t"));
    }

    #[test]
    fn test_wildcard() {
        assert!(matches("cat", "c.t"));
        assert!(!matches("car", "c.t"));
        assert!(matches("goøö0Ogol", "g.+gol"));
        assert!(!matches("gol", "g.+gol"));
    }

    #[test]
    fn test_alternation() {
        assert!(matches("a cat", "a (cat|dog)"));
        assert!(!matches("a cow", "a (cat|dog)"));
        assert!(matches(
            "I see 1 cat, 2 dogs and 3 cows",
            "^I see (\\d (cat|dog|cow)s?(, | and )?)+$"
        ));
        assert!(!matches(
            "I see 1 cat, 2 dogs and 3 cows",
            "^I see (\\d (cat|dog|cow)(, | and )?)+$"
        ));
    }
}
