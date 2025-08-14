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
enum State {
    Char(char),
    Class(Meta),
    Bound(Meta),
    CharGroup { chars: Vec<State>, negate: bool },
    CaptureGroup { groups: Vec<Vec<State>>, index: usize },
    Quantifier { state: Box<State>, kind: Meta },
    BackRef(usize),
}


// Returns an AST form of the regex
fn compile_regex(regex: &str, capture_index: &mut usize) -> Vec<State> {
    let mut iter = regex.char_indices();
    let mut patterns = Vec::new();
    let mut num_capture_groups = 0;

    while let Some((i, c)) = iter.next() {
        match c {
            '\\' => match iter.next() {
                Some((_, 'd')) => patterns.push(State::Class(Meta::Digit)),
                Some((_, 'w')) => patterns.push(State::Class(Meta::Alphanumeric)),
                Some((_, other)) if other.is_numeric() => {
                    let index = other.to_digit(10).unwrap() as usize;
                    if index == 0 {
                        panic!("backref are 1-indexed, upto 9")
                    }
                    if index > num_capture_groups {
                        panic!("invalid backreference")
                    }
                    patterns.push(State::BackRef(index))
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
                    let group = compile_regex(&subpattern[start..], capture_index);
                    patterns.push(State::CharGroup { chars: group, negate });
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
                    let this_index = *capture_index;
                    *capture_index += 1;
                    let alternates = alternates
                        .iter()
                        .map(|regex| compile_regex(regex, capture_index))
                        .collect();
                    patterns.push(State::CaptureGroup {
                        groups: alternates,
                        index: this_index,
                    });
                    num_capture_groups += 1;
                }
            }
            '^' if i == 0 => {
                patterns.push(State::Bound(Meta::Start));
            }
            '$' if i == regex.len() - 1 => {
                patterns.push(State::Bound(Meta::End));
            }
            '+' | '*' | '?' => {
                let Some(pattern) = patterns.pop() else {
                    panic!("quantifier missing pattern");
                };
                patterns.push(State::Quantifier {
                    state: Box::new(pattern),
                    kind: match c {
                        '+' => Meta::OneOrMore,
                        '*' => Meta::ZeroOrMore,
                        '?' => Meta::ZeroOrOne,
                        _ => unreachable!(),
                    },
                });
            }
            _ => {
                patterns.push(State::Char(c));
            }
        }
    }
    patterns
}

fn match_pattern(text: &str, regex: &str) -> Option<usize> {
    let text = text.chars().collect::<Vec<char>>();
    let regex = compile_regex(regex, &mut 1);
    eprintln!("{:?} ({})", text, text.len());
    eprintln!("{:#?} ({})\n", regex, regex.len());

    None
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
    if args.recursive && (args.paths.len() != 1 || !path::Path::new(&args.paths[0]).is_dir()) {
        eprintln!("Error: -r requires exactyly one path to a directory.");
        process::exit(1);
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
                        print_match(input, Some(fp), show_prefix);
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
        assert!(!matches("act", "ca+t"));
        assert!(!matches("ca", "ca+t"));
        assert!(matches("a", "a+"));
        assert!(matches("aa", "a+a"));
        assert!(matches("aa", "aa+"));
    }

    #[test]
    fn test_zero_or_more_times() {
        assert!(matches("cat", "ca*t"));
        assert!(matches("caaats", "ca*t"));
        assert!(!matches("act", "ca+t"));
        assert!(!matches("ca", "ca+t"));
        assert!(matches("a", "a*"));
        assert!(matches("aa", "a*a"));
        assert!(matches("aa", "aa*"));
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

    #[test]
    fn test_backreference_basic() {
        assert!(matches("cat and cat", "(cat) and \\1"));
        assert!(!matches("cat and dog", "(cat) and \\1"));
    }

    #[test]
    fn test_backreference_complex_word_digit() {
        assert!(matches(
            "grep 101 is doing grep 101 times",
            "(\\w\\w\\w\\w \\d\\d\\d) is doing \\1 times"
        ));
        assert!(!matches(
            "$?! 101 is doing $?! 101 times",
            "(\\w\\w\\w \\d\\d\\d) is doing \\1 times"
        ));
        assert!(!matches(
            "grep yes is doing grep yes times",
            "(\\w\\w\\w\\w \\d\\d\\d) is doing \\1 times"
        ));
    }

    #[test]
    fn test_backreference_char_group() {
        assert!(matches(
            "abcd is abcd, not efg",
            "([abcd]+) is \\1, not [^xyz]+"
        ));
        assert!(!matches(
            "efgh is efgh, not efg",
            "([abcd]+) is \\1, not [^xyz]+"
        ));
        assert!(!matches(
            "abcd is abcd, not xyz",
            "([abcd]+) is \\1, not [^xyz]+"
        ));
    }

    #[test]
    fn test_backreference_anchors() {
        assert!(matches(
            "this starts and ends with this",
            "^(\\w+) starts and ends with \\1$"
        ));
        assert!(!matches(
            "that starts and ends with this",
            "^(this) starts and ends with \\1$"
        ));
        assert!(!matches(
            "this starts and ends with this?",
            "^(this) starts and ends with \\1$"
        ));
    }

    #[test]
    fn test_backreference_quantifier() {
        assert!(matches(
            "once a dreaaamer, always a dreaaamer",
            "once a (drea+mer), alwaysz? a \\1"
        ));
        assert!(!matches(
            "once a dremer, always a dreaaamer",
            "once a (drea+mer), alwaysz? a \\1"
        ));
        assert!(!matches(
            "once a dreaaamer, alwayszzz a dreaaamer",
            "once a (drea+mer), alwaysz? a \\1"
        ));
    }

    #[test]
    fn test_backreference_alternation() {
        assert!(matches(
            "bugs here and bugs there",
            "(b..s|c..e) here and \\1 there"
        ));
        assert!(!matches(
            "bugz here and bugs there",
            "(b..s|c..e) here and \\1 there"
        ));
    }
}
