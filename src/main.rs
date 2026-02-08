mod ast;
mod nfa;

use clap::{Parser, ValueEnum};
use std::cmp::max;
use std::collections::HashMap;
use std::fs;
use std::io;
use std::io::IsTerminal;
use std::io::Read;
use std::path;
use std::process;
use std::rc::Rc;
use std::vec;
use walkdir::WalkDir;

// Helpers

fn find_files_recursive(root: &str) -> Vec<String> {
    WalkDir::new(root)
        .into_iter()
        .filter_map(Result::ok)
        .filter(|entry| entry.file_type().is_file())
        .map(|entry| entry.path().display().to_string())
        .collect()
}

fn get_inputs(args: &Args) -> HashMap<String, String> {
    let mut inputs: HashMap<String, String> = HashMap::new();
    if args.paths.is_empty() {
        // read from stdin
        let mut text = String::new();
        io::stdin()
            .read_to_string(&mut text)
            .expect("unable to read from stdin");
        inputs.insert(String::from("stdin"), text);
    } else {
        // read from files on disk
        let filepaths = if args.recursive {
            find_files_recursive(&args.paths[0])
        } else {
            args.paths.clone()
        };
        for fp in filepaths {
            // eprintln!("Processing {:?}...", fp);
            let text = fs::read_to_string(&fp).expect("unable to open file");
            inputs.insert(fp, text);
        }
    }
    inputs
}

fn print_match(
    pmatches: Vec<nfa::Match>,
    line: &str,
    fp: &str,
    only_matching: bool,
    show_prefix: bool,
    use_color: bool,
) {
    if show_prefix {
        print!("{}:", fp);
    }
    if only_matching {
        for pmatch in pmatches {
            println!("{}", pmatch.text);
        }
    } else if use_color {
        let mut result = String::new();
        let mut last_pos = 0;
        for pmatch in pmatches {
            result.push_str(&line[last_pos..pmatch.start]);
            result.push_str(&format!("\x1b[01;31m{}\x1b[m", pmatch.text));
            last_pos = pmatch.end;
        }
        result.push_str(&line[last_pos..]);
        println!("{}", result);
    } else {
        println!("{}", line);
    }
}

/// Checks a regular expression against a single line
fn match_pattern(line: &str, regex: &str) -> Option<Vec<nfa::Match>> {
    let mut chars: Vec<char> = line.chars().collect();
    let ast = ast::string_to_ast(regex, &mut 1);
    let (nfa, _) = nfa::ast_to_nfa(&ast);

    // Modify input text to include start and end markers
    if let Some(ast::Token::Bound(ast::Meta::End)) = ast.last() {
        chars.push(nfa::END_OF_TEXT);
    }
    if let Some(ast::Token::Bound(ast::Meta::Start)) = ast.first() {
        chars.insert(0, nfa::START_OF_TEXT);
        // Simulate the ε-NFA only from the start of the text
        let mut context = nfa::MatchContext::new(0);
        return nfa::simulate_nfa(nfa, &chars, 0, &mut context).then(|| vec![context.get(&chars)]);
    }

    // Simulate the ε-NFA for each position in the text
    let mut hits = Vec::new();
    let mut i = 0;

    while i < chars.len() {
        let mut context = nfa::MatchContext::new(i);
        if nfa::simulate_nfa(Rc::clone(&nfa), &chars, i, &mut context) {
            let pmatch = context.get(&chars);
            i += max(pmatch.text.len(), 1);
            hits.push(pmatch);
        } else {
            i += 1
        }
    }

    (!hits.is_empty()).then_some(hits)
}

// Argument Parsing

#[derive(Parser, Debug)]
struct Args {
    #[arg(short = 'E', short_alias = 'P')]
    expression: String,

    #[arg(short = 'r')]
    recursive: bool,

    #[arg(short = 'o')]
    only_matching: bool,

    #[arg(long, value_enum, default_value = "auto")]
    color: ColorMode,

    paths: Vec<String>,
}

#[derive(ValueEnum, Clone, Debug)]
enum ColorMode {
    Always,
    Never,
    Auto,
}

// Main

fn main() {
    let args = Args::parse();
    if args.recursive && (args.paths.len() != 1 || !path::Path::new(&args.paths[0]).is_dir()) {
        eprintln!("Error: -r requires exactyly one path to a directory.");
        process::exit(1);
    }
    let inputs = get_inputs(&args);
    let show_prefix = inputs.len() > 1 || args.recursive;
    let use_color: bool = match args.color {
        ColorMode::Always => true,
        ColorMode::Never => false,
        ColorMode::Auto => io::stdout().is_terminal(),
    };
    let only_matching = args.only_matching;
    let mut success = false;

    for (fp, input) in inputs {
        for line in input.lines() {
            if let Some(pmatches) = match_pattern(line, &args.expression) {
                print_match(pmatches, line, &fp, only_matching, show_prefix, use_color);
                success = true;
            }
        }
    }

    process::exit((!success) as i32)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn matches(text: &str, regex: &str) -> bool {
        match_pattern(text, regex).is_some()
    }

    fn get_first_match(text: &str, regex: &str) -> String {
        match_pattern(text, regex).unwrap()[0].text.clone()
    }

    fn get_all_matches(text: &str, regex: &str) -> Vec<String> {
        match_pattern(text, regex)
            .unwrap()
            .iter()
            .map(|m| m.text.clone())
            .collect()
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
    fn test_digit_edge_cases() {
        assert!(matches("0", r"\d"));
        assert!(matches("9", r"\d"));
        assert!(!matches(".", r"\d"));
        assert!(!matches("-", r"\d"));
        assert!(!matches("+", r"\d"));
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
    fn test_alphanumeric_edge_cases() {
        assert!(matches("_", r"\w"));
        assert!(!matches(" ", r"\w"));
        assert!(!matches("\n", r"\w"));
        assert!(!matches("\t", r"\w"));
    }

    #[test]
    fn test_positive_character_groups() {
        assert!(matches("l", "[apple]"));
        assert!(!matches("banana", "[cdefgh]"));
        assert!(!matches("[]", "[pineapple]"));
    }

    #[test]
    fn test_character_group_edge_cases() {
        assert!(matches("a", "[a]"));
        assert!(!matches("b", "[a]"));
        assert!(matches("a", "[aaa]"));
    }

    #[test]
    fn test_negative_character_groups() {
        assert!(matches("apple", "[^xyz]"));
        assert!(matches("apple", "[^abc]"));
        assert!(!matches("banana", "[^anb]"));
        assert!(matches("orange", "[^opq]"));
    }

    #[test]
    fn test_negative_character_group_edge_cases() {
        assert!(!matches("a", "[^a]"));
        assert!(matches("b", "[^a]"));
        assert!(!matches("x", "[^xyz]"));
        assert!(matches("a", "[^xyz]"));
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
    fn test_mixed_character_classes() {
        assert!(matches("a1", r"\w\d"));
        assert!(matches("1a", r"\d\w"));
        assert!(!matches("ab", r"\w\d"));
        assert!(matches("a1", r"\w[123]"));
        assert!(!matches("a4", r"\w[123]"));
    }

    #[test]
    fn test_start_of_string_anchor() {
        assert!(matches("log", "^log"));
        assert!(!matches("slog", "^log"));
    }

    #[test]
    fn test_start_anchor_edge_cases() {
        assert!(matches("anything", "^"));
        assert!(matches("aaa", "^a+"));
        assert!(!matches("baaa", "^a+"));
        assert!(matches("123-456-7890", r"^\d{2,3}-\d{3}-\d{4}"));
        assert!(!matches("1234-456-7890", r"^\d{2,3}-\d{3}-\d{4}"));
    }

    #[test]
    fn test_end_of_string_anchor() {
        assert!(matches("cat", "cat$"));
        assert!(!matches("cats", "cat$"));
    }

    #[test]
    fn test_end_anchor_edge_cases() {
        assert!(matches("anything", "$"));
        assert!(matches("aaa", "a+$"));
        assert!(!matches("aaab", "a+$"));
        assert!(matches("ahello", "a{0,2}hello$"));
        assert!(matches("aaahello", "a{0,2}hello$"));
    }

    #[test]
    fn test_both_anchors() {
        assert!(matches("hello", "^hello$"));
        assert!(!matches("hello world", "^hello$"));
        assert!(!matches("say hello", "^hello$"));
        assert!(!matches("a", "^$"));
        assert!(matches("catcatcatcat", "^(cat){2,4}$"));
        assert!(!matches("catcatcatcatcat", "^(cat){2,4}$"));
        assert!(matches("123-456-7890", r"^\d{2,3}-\d{3}-\d{4}$"));
        assert!(matches("12-456-7890", r"^\d{2,3}-\d{3}-\d{4}$"));
        assert!(!matches("1234-456-7890", r"^\d{2,3}-\d{3}-\d{4}$"));
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
    fn test_one_or_more_edge_cases() {
        assert!(matches("123", r"\d+"));
        assert!(matches("1", r"\d+"));
        assert!(matches("aaa", "[abc]+"));
        assert!(!matches("xyz", "[abc]+"));
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
    fn test_zero_or_more_edge_cases() {
        assert!(matches("b", "a*"));
        assert!(matches("hello", "a*hello"));
        assert!(matches("abc", r"\d*"));
    }

    #[test]
    fn test_zero_or_one_times() {
        assert!(matches("cat", "ca?t"));
        assert!(matches("act", "ca?t"));
        assert!(!matches("dog", "ca?t"));
        assert!(!matches("cag", "ca?t"));
    }

    #[test]
    fn test_zero_or_one_edge_cases() {
        assert!(matches("a1", r"\w?\d"));
        assert!(matches("1", r"\w?\d"));
        assert!(!matches("ab", r"\w?\d"));
        assert!(matches("hello", "a?hello"));
        assert!(matches("ahello", "a?hello"));
        assert!(matches("aahello", "a?hello"));
    }

    #[test]
    fn test_exact_count_quantifier() {
        assert!(matches("caaat", "ca{3}t"));
        assert!(!matches("caat", "ca{3}t"));
        assert!(!matches("caaaat", "ca{3}t"));
        assert!(matches("d42g", r"d\d{2}g"));
        assert!(!matches("d1g", r"d\d{2}g"));
        assert!(!matches("d123g", r"d\d{2}g"));
        assert!(matches("czyxzw", "c[xyz]{4}w"));
        assert!(!matches("cxyzw", "c[xyz]{4}w"));
    }

    #[test]
    fn test_exact_count_edge_cases() {
        assert!(matches("ct", "ca{0}t"));
        assert!(!matches("cat", "ca{0}t"));
        assert!(matches("cat", "ca{1}t"));
        assert!(!matches("caat", "ca{1}t"));
        assert!(matches("aaaaaaaaaa", "a{10}"));
    }

    #[test]
    fn test_range_quantifier() {
        assert!(matches("caat", "ca{2,4}t"));
        assert!(matches("caaat", "ca{2,4}t"));
        assert!(matches("caaaat", "ca{2,4}t"));
        assert!(!matches("cat", "ca{2,4}t"));
        assert!(!matches("caaaaat", "ca{2,4}t"));
        assert!(matches("n123m", r"n\d{1,3}m"));
        assert!(!matches("n1234m", r"n\d{1,3}m"));
        assert!(matches("pxyq", "p[xyz]{2,3}q"));
        assert!(matches("pxyzq", "p[xyz]{2,3}q"));
        assert!(!matches("pxq", "p[xyz]{2,3}q"));
        assert!(!matches("pxyzyq", "p[xyz]{2,3}q"));
    }

    #[test]
    fn test_range_quantifier_edge_cases() {
        assert!(matches("ct", "ca{0,2}t"));
        assert!(matches("cat", "ca{0,2}t"));
        assert!(matches("caat", "ca{0,2}t"));
        assert!(!matches("caaat", "ca{0,2}t"));
        assert!(matches("hello", "a{0,5}hello"));
        assert!(matches("aahello", "a{0,5}hello"));
        assert!(matches("aaahello", "a{0,2}hello"));
    }

    #[test]
    fn test_unbounded_quantifier() {
        assert!(matches("caat", "ca{2,}t"));
        assert!(matches("caaat", "ca{2,}t"));
        assert!(matches("caaaaaaat", "ca{2,}t"));
        assert!(!matches("cat", "ca{2,}t"));
        assert!(matches("x9999y", r"x\d{3,}y"));
        assert!(!matches("x42y", r"x\d{3,}y"));
        assert!(matches("baeiour", "b[aeiou]{2,}r"));
        assert!(!matches("bar", "b[aeiou]{2,}r"));
    }

    #[test]
    fn test_unbounded_quantifier_edge_cases() {
        assert!(matches("hello", "a{0,}hello"));
        assert!(matches("aaaaaahello", "a{0,}hello"));
        assert!(matches("aaaaaaaaaa", "a{1,}"));
        assert!(!matches("", "a{1,}"));
    }

    #[test]
    fn test_wildcard() {
        assert!(matches("cat", "c.t"));
        assert!(!matches("car", "c.t"));
        assert!(matches("goøö0Ogol", "g.+gol"));
        assert!(!matches("gol", "g.+gol"));
    }

    #[test]
    fn test_wildcard_edge_cases() {
        assert!(matches("c\nt", "c.t"));
        assert!(matches("c\tt", "c.t"));
        assert!(matches("c t", "c.t"));
        assert!(matches("anything", ".*"));
        assert!(matches("a", ".+"));
    }

    #[test]
    fn test_wildcard_with_anchors() {
        assert!(!matches("^", "."));
        assert!(!matches("$", "."));
        assert!(matches("abc", "^.+$"));
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
    fn test_alternation_edge_cases() {
        assert!(matches("a", "(a|b)"));
        assert!(matches("b", "(a|b)"));
        assert!(!matches("c", "(a|b)"));
    }

    #[test]
    fn test_nested_groups() {
        assert!(matches("abc", "((a)bc)"));
        assert!(!matches("xbc", "((a)bc)"));
        assert!(matches("cat", "(c(a|o)t)"));
        assert!(matches("cot", "(c(a|o)t)"));
        assert!(!matches("cit", "(c(a|o)t)"));
    }

    #[test]
    fn test_quantified_groups() {
        assert!(matches("abab", "(ab)+"));
        assert!(matches("ab", "(ab)+"));
        assert!(!matches("a", "(ab)+"));
        assert!(matches("hello", "(hi)?hello"));
        assert!(matches("hihello", "(hi)?hello"));
        assert!(matches("catcatcat", "(cat){3}"));
        assert!(!matches("catcat", "(cat){3}"));
        assert!(matches("catcat", "(cat){2,4}"));
        assert!(matches("catcatcatcat", "(cat){2,4}"));
        assert!(!matches("cat", "(cat){2,4}"));
        assert!(matches("catcatcatcatcat", "(cat){2,4}"));
    }

    #[test]
    fn test_complex_patterns() {
        assert!(matches("user@domain.com", r"\w+@\w+\.\w+"));
        assert!(!matches("user.domain.com", r"\w+@\w+\.\w+"));
        assert!(matches("123-456-7890", r"\d\d\d-\d\d\d-\d\d\d\d"));
        assert!(!matches("12-456-7890", r"\d\d\d-\d\d\d-\d\d\d\d"));
        assert!(matches("123-456-7890", r"\d{3}-\d{3}-\d{4}"));
        assert!(!matches("12-456-7890", r"\d{3}-\d{3}-\d{4}"));
        assert!(matches("12-456-7890", r"\d{2,3}-\d{3}-\d{4}"));
        assert!(matches("123-45-6789", r"\d{2,3}-\d{2,3}-\d{4}"));
        assert!(matches("1234-456-7890", r"\d{2,3}-\d{3}-\d{4}"));
    }

    #[test]
    fn test_multiple_quantifiers() {
        assert!(matches("aabb", "a+b+"));
        assert!(matches("aaabbb", "a+b+"));
        assert!(matches("ab", "a+b+"));
        assert!(matches("abc", "a?b*c+"));
        assert!(matches("bcccc", "a?b*c+"));
        assert!(!matches("ab", "a?b*c+"));
        assert!(matches("aabbcc", "a{2}b{2}c{2}"));
        assert!(!matches("abbc", "a{2}b{2}c{2}"));
        assert!(matches("bccccdd", "a?b*c+d{2}"));
        assert!(!matches("abcd", "a?b*c+d{2}"));
        assert!(matches("aabbbcc", "a{2}b{2,4}c{2}"));
        assert!(matches("aabbbbcc", "a{2}b{2,4}c{2}"));
        assert!(!matches("aabbbbbcc", "a{2}b{2,4}c{2}"));
    }

    #[test]
    fn test_edge_case_combinations() {
        assert!(matches("aaaa", "^a+$"));
        assert!(!matches("baaa", "^a+$"));
        assert!(matches("1", r"(\d|[abc])"));
        assert!(matches("a", r"(\d|[abc])"));
        assert!(!matches("x", r"(\d|[abc])"));
        assert!(matches("aaaa", "^a{4}$"));
        assert!(!matches("aaa", "^a{4}$"));
        assert!(matches("aaa", "^a{2,5}$"));
        assert!(matches("aaaa", "^a{2,5}$"));
        assert!(!matches("aaaaaa", "^a{2,5}$"));
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

    #[test]
    fn test_nested_backreferences() {
        // Test basic nested backreferences
        assert!(matches("abaab", "((a)b)\\2\\1")); // ab + a + ab = abaab
        assert!(!matches("abab", "((a)b)\\2\\1")); // ab + a + ab ≠ abab
        assert!(!matches("abba", "((a)b)\\2\\1")); // ab + a + ab ≠ abba

        // Test multiple levels of nesting
        assert!(matches("abccbcabc", "(a(b(c)))\\3\\2\\1")); // abc + c + bc + abc
        assert!(!matches("abcabc", "(a(b(c)))\\3\\2\\1")); // not enough chars
    }

    #[test]
    fn test_complex_nested_backreferences() {
        // Test nested groups with alternation
        assert!(matches("abxbabx", "(a(b|c)x)\\2\\1")); // abx + b + abx
        assert!(matches("acxcacx", "(a(b|c)x)\\2\\1")); // acx + c + acx
        assert!(!matches("abxcabx", "(a(b|c)x)\\2\\1")); // mixed b/c
        assert!(!matches("abxaby", "(a(b|c)x)\\2\\1")); // not enough chars
    }

    #[test]
    fn test_multiple_backreferences() {
        // Test multiple backreferences in one pattern
        assert!(matches(
            "3 red squares and 3 red circles",
            "(\\d+) (\\w+) squares and \\1 \\2 circles"
        ));
        assert!(!matches(
            "3 red squares and 4 red circles",
            "(\\d+) (\\w+) squares and \\1 \\2 circles"
        ));
        assert!(matches(
            "grep 101 is doing grep 101 times",
            "(\\w\\w\\w\\w) (\\d\\d\\d) is doing \\1 \\2 times"
        ));
        assert!(!matches(
            "$?! 101 is doing $?! 101 times",
            "(\\w\\w\\w) (\\d\\d\\d) is doing \\1 \\2 times"
        ));
        assert!(!matches(
            "grep yes is doing grep yes times",
            "(\\w\\w\\w\\w) (\\d\\d\\d) is doing \\1 \\2 times"
        ));
    }

    #[test]
    fn test_multiple_backreferences_with_character_groups() {
        // Test multiple backreferences with character groups
        assert!(matches(
            "abc-def is abc-def, not efg",
            "([abc]+)-([def]+) is \\1-\\2, not [^xyz]+"
        ));
        assert!(!matches(
            "efg-hij is efg-hij, not efg",
            "([abc]+)-([def]+) is \\1-\\2, not [^xyz]+"
        ));
        assert!(!matches(
            "abc-def is abc-def, not xyz",
            "([abc]+)-([def]+) is \\1-\\2, not [^xyz]+"
        ));
    }

    #[test]
    fn test_multiple_backreferences_with_anchors() {
        // Test multiple backreferences with anchors
        assert!(matches(
            "apple pie, apple and pie",
            "^(\\w+) (\\w+), \\1 and \\2$"
        ));
        assert!(!matches(
            "pineapple pie, pineapple and pie",
            "^(apple) (\\w+), \\1 and \\2$"
        ));
        assert!(!matches(
            "apple pie, apple and pies",
            "^(\\w+) (pie), \\1 and \\2$"
        ));
    }

    #[test]
    fn test_multiple_backreferences_with_quantifiers() {
        // Test multiple backreferences with quantifiers
        assert!(matches(
            "howwdy hey there, howwdy hey",
            "(how+dy) (he?y) there, \\1 \\2"
        ));
        assert!(!matches(
            "hody hey there, howwdy hey",
            "(how+dy) (he?y) there, \\1 \\2"
        ));
        assert!(!matches(
            "howwdy heeey there, howwdy heeey",
            "(how+dy) (he?y) there, \\1 \\2"
        ));
    }

    #[test]
    fn test_multiple_backreferences_with_alternation() {
        // Test multiple backreferences with alternation
        assert!(matches(
            "cat and fish, cat with fish",
            "(c.t|d.g) and (f..h|b..d), \\1 with \\2"
        ));
        assert!(!matches(
            "bat and fish, cat with fish",
            "(c.t|d.g) and (f..h|b..d), \\1 with \\2"
        ));
    }

    #[test]
    fn test_advanced_nested_backreferences() {
        // Test complex nested backreferences from the actual test cases
        assert!(matches(
            "'cat and cat' is the same as 'cat and cat'",
            "('(cat) and \\2') is the same as \\1"
        ));
        assert!(!matches(
            "'cat and cat' is the same as 'cat and dog'",
            "('(cat) and \\2') is the same as \\1"
        ));

        assert!(matches(
            "grep 101 is doing grep 101 times, and again grep 101 times",
            "((\\w\\w\\w\\w) (\\d\\d\\d)) is doing \\2 \\3 times, and again \\1 times"
        ));
        assert!(!matches(
            "$?! 101 is doing $?! 101 times, and again $?! 101 times",
            "((\\w\\w\\w) (\\d\\d\\d)) is doing \\2 \\3 times, and again \\1 times"
        ));
        assert!(!matches(
            "grep yes is doing grep yes times, and again grep yes times",
            "((\\w\\w\\w\\w) (\\d\\d\\d)) is doing \\2 \\3 times, and again \\1 times"
        ));
    }

    #[test]
    fn test_deeply_nested_backreferences() {
        // Test deeply nested patterns with multiple groups and character classes
        assert!(matches(
            "abc-def is abc-def, not efg, abc, or def",
            "(([abc]+)-([def]+)) is \\1, not ([^xyz]+), \\2, or \\3"
        ));
        assert!(!matches(
            "efg-hij is efg-hij, not klm, efg, or hij",
            "(([abc]+)-([def]+)) is \\1, not ([^xyz]+), \\2, or \\3"
        ));
        assert!(!matches(
            "abc-def is abc-def, not xyz, abc, or def",
            "(([abc]+)-([def]+)) is \\1, not ([^xyz]+), \\2, or \\3"
        ));
    }

    #[test]
    fn test_complex_nested_with_anchors() {
        // Test complex nested backreferences with anchors
        assert!(matches(
            "apple pie is made of apple and pie. love apple pie",
            "^((\\w+) (\\w+)) is made of \\2 and \\3. love \\1$"
        ));
        assert!(!matches(
            "pineapple pie is made of apple and pie. love apple pie",
            "^((apple) (\\w+)) is made of \\2 and \\3. love \\1$"
        ));
        assert!(!matches(
            "apple pie is made of apple and pie. love apple pies",
            "^((\\w+) (pie)) is made of \\2 and \\3. love \\1$"
        ));
    }

    #[test]
    fn test_complex_nested_with_quantifiers() {
        // Test complex nested backreferences with quantifiers
        assert!(matches(
            "'howwdy hey there' is made up of 'howwdy' and 'hey'. howwdy hey there",
            "'((how+dy) (he?y) there)' is made up of '\\2' and '\\3'. \\1"
        ));
        assert!(!matches(
            "'hody hey there' is made up of 'hody' and 'hey'. hody hey there",
            "'((how+dy) (he?y) there)' is made up of '\\2' and '\\3'. \\1"
        ));
        assert!(!matches(
            "'howwdy heeey there' is made up of 'howwdy' and 'heeey'. howwdy heeey there",
            "'((how+dy) (he?y) there)' is made up of '\\2' and '\\3'. \\1"
        ));
    }

    #[test]
    fn test_complex_nested_with_alternation() {
        // Test complex nested backreferences with alternation
        assert!(matches(
            "cat and fish, cat with fish, cat and fish",
            "((c.t|d.g) and (f..h|b..d)), \\2 with \\3, \\1"
        ));
        assert!(!matches(
            "bat and fish, bat with fish, bat and fish",
            "((c.t|d.g) and (f..h|b..d)), \\2 with \\3, \\1"
        ));
    }

    #[test]
    fn test_print_single_matching_line() {
        // From Stage #KU5 - Print a single matching line
        assert_eq!(get_first_match("banana123", r"\d"), '1'.to_string());
        assert!(match_pattern("cherry", r"\d").is_none());
        assert_eq!(
            get_first_match("pineapple_suffix", "^pineapple"),
            "pineapple".to_string()
        );
        assert!(match_pattern("prefix_pineapple", "^pineapple").is_none());
        assert_eq!(get_first_match("cat", "ca+t"), "cat".to_string());
        assert_eq!(get_first_match("otest", "[orange]"), 'o'.to_string());
    }

    #[test]
    fn test_print_multiple_matching_lines() {
        // From Stage #SS2 - Print multiple matches
        assert_eq!(get_all_matches("a1b2c3", r"\d"), vec!["1", "2", "3"]);
        assert!(match_pattern("cherry", r"\d").is_none());
        assert_eq!(
            get_all_matches("cherry_watermelon_grape", "(grape|watermelon|cherry)"),
            vec!["cherry", "watermelon", "grape"]
        );
        assert_eq!(
            get_all_matches("xx, yy, zz", r"\w\w"),
            vec!["xx", "yy", "zz"]
        );
        assert!(match_pattern("##$$%", r"\w").is_none());
        assert_eq!(
            get_all_matches(
                "I see 3 elephants. Also, I see 4 monkeys.",
                r"I see \d+ (elephant|monkey)s?"
            ),
            vec!["I see 3 elephants", "I see 4 monkeys"]
        );
    }

    #[test]
    fn test_process_multiple_input_lines() {
        // From Stage #BO4 - Process multiple input lines
        assert_eq!(
            get_all_matches("a1b\nno digits here\n2c3d", r"\d"),
            vec!["1", "2", "3"]
        );
        assert_eq!(
            get_all_matches(
                "I like lemon\nnothing here\nblueberry and lemon are tasty",
                "(lemon|blueberry)"
            ),
            vec!["lemon", "blueberry", "lemon"]
        );
        assert!(match_pattern("abc\ndef\nghi", "XYZ123").is_none());
        assert_eq!(
            get_all_matches("dogdogdog\nelephant\ncat-cat-dog", "cat"),
            vec!["cat", "cat"]
        );
        assert_eq!(
            get_all_matches("Yesterday I saw 3 tiger and I saw 45 panda.\nNothing interesting today.\nLast week I saw 12 pandas.", r"I saw \d+ (tiger|panda)s?"),
            vec!["I saw 3 tiger", "I saw 45 panda", "I saw 12 pandas"]
        );
        assert!(match_pattern(
            "today is sunny\nno rains here\ntomorrow maybe rainy",
            "cats and dogs"
        )
        .is_none());
    }
}
