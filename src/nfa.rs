use crate::ast::{Meta, Token};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

// Special transition characters
const EPSILON: char = 'ε';
const ANY: char = '.';
pub const START_OF_TEXT: char = '^';
pub const END_OF_TEXT: char = '$';

// Special characters for backreference tracking
const CAPTURE_START_BASE: char = '\u{E000}';
const CAPTURE_END_BASE: char = '\u{E100}';
const BACKREF_BASE: char = '\u{F000}';

// States in the ε-Non-deterministic Finite Automata
#[derive(Debug, Default)]
pub struct State {
    initial: bool,
    terminal: bool,
    transitions: HashMap<char, Vec<StatePtr>>, // edge_char -> [next_states]
}

// Capture tracking for backreferences
#[derive(Debug, Default, Clone)]
pub struct MatchContext {
    captures: HashMap<usize, String>, // capture_index -> capture_string
    active_captures: HashMap<usize, usize>, // capture_index -> start_pos
}

type StatePtr = Rc<RefCell<State>>;

// Parses an AST into an ε-NFA
pub fn ast_to_nfa(ast: &[Token]) -> (StatePtr, StatePtr) {
    // Convert all tokens to ε-NFA fragments and joins theme with ε-transitions
    let (start_state, mut end_state) = token_to_nfa(&ast[0]);
    for token in ast.iter().skip(1) {
        let (start_next, end_next) = token_to_nfa(token);
        add_transition(EPSILON, &end_state, &start_next);
        end_state = end_next;
    }
    // Creates the initial and terminal states and connects them to the ε-NFA above
    let start = Rc::new(RefCell::new(State {
        initial: true,
        transitions: HashMap::from([(EPSILON, vec![Rc::clone(&start_state)])]),
        ..State::default()
    }));
    let end = Rc::new(RefCell::new(State {
        terminal: true,
        ..State::default()
    }));
    add_transition(EPSILON, &end_state, &end);
    // The ε-NFA is represented by the start state
    (start, end)
}

// Parses a token into an ε-NFA fragment
fn token_to_nfa(token: &Token) -> (StatePtr, StatePtr) {
    let (start, end) = (
        Rc::new(RefCell::new(State::default())),
        Rc::new(RefCell::new(State::default())),
    );
    match token {
        // Transitions (edges) are represented by characters
        Token::Char(c) => add_transition(*c, &start, &end),
        // Adds one transition for each character in the class
        Token::Class(cls) => match cls {
            Meta::Digit => {
                for c in '0'..='9' {
                    add_transition(c, &start, &end);
                }
            }
            Meta::Alphanumeric => {
                for c in ('a'..='z')
                    .chain('A'..='Z')
                    .chain('0'..='9')
                    .chain(std::iter::once('_'))
                {
                    add_transition(c, &start, &end);
                }
            }
            Meta::Wildcard => {
                add_transition(ANY, &start, &end);
            }
            _ => unreachable!(),
        },
        // Adds one transition for each character in the character group, and an ANY transition
        // if negate is true
        Token::CharGroup { chars, negate } => {
            // The characters in the group lead to a deadend state, and there is an ANY transition
            // to the end state. This complexity is handled in the simulation.
            if *negate {
                add_transition(ANY, &start, &end);
                let deadend = Rc::new(RefCell::new(State::default()));
                for token in chars {
                    if let Token::Char(c) = token {
                        add_transition(*c, &start, &deadend);
                    } else {
                        panic!("[^...] group must only contain literal characters")
                    }
                }
            // The characters in the group lead to the end state directly
            } else {
                for token in chars {
                    if let Token::Char(c) = token {
                        add_transition(*c, &start, &end);
                    } else {
                        panic!("[...] group must only contains literal characters")
                    }
                }
            }
        }
        // These special characters are handled during simulation by adding them to the input
        Token::Bound(bound) => match bound {
            Meta::Start => add_transition(START_OF_TEXT, &start, &end),
            Meta::End => add_transition(END_OF_TEXT, &start, &end),
            _ => unreachable!(),
        },
        // Handles both alternation and actually capturing the capture groups for backreferences
        Token::CaptureGroup { alternates, index } => {
            // Add capture start marker with encoded index
            let cap_start = Rc::new(RefCell::new(State::default()));
            let cap_end = Rc::new(RefCell::new(State::default()));

            // Encode capture index in the marker character
            let start_marker = char::from_u32(CAPTURE_START_BASE as u32 + *index as u32).unwrap();
            let end_marker = char::from_u32(CAPTURE_END_BASE as u32 + *index as u32).unwrap();

            add_transition(start_marker, &start, &cap_start);

            // Each alternate is a separate ε-NFA fragment, things are joined as follows:
            // start -> cap_start (-> alt_start -> ... -> alt_end ->)+ cap_end -> end
            for ast in alternates {
                let (alt_start, mut alt_end) = if ast.is_empty() {
                    (Rc::clone(&cap_start), Rc::clone(&cap_start))
                } else {
                    token_to_nfa(&ast[0])
                };
                for token in ast.iter().skip(1) {
                    let (start_next, end_next) = token_to_nfa(token);
                    add_transition(EPSILON, &alt_end, &start_next);
                    alt_end = end_next;
                }
                add_transition(EPSILON, &cap_start, &alt_start);
                add_transition(end_marker, &alt_end, &cap_end);
            }

            add_transition(EPSILON, &cap_end, &end);
        }
        // ε-transition from start to end enables 0 matches, omiting it enables at least 1 match
        // ε-transition from token_end to token_start enables ∞ matches
        Token::Quantifier { token, kind } => match kind {
            Meta::ZeroOrOne => {
                add_transition(EPSILON, &start, &end);
                let (token_start, token_end) = token_to_nfa(token);
                add_transition(EPSILON, &start, &token_start);
                add_transition(EPSILON, &token_end, &end);
            }
            Meta::OneOrMore => {
                let (token_start, token_end) = token_to_nfa(token);
                add_transition(EPSILON, &start, &token_start);
                add_transition(EPSILON, &token_end, &end);
                add_transition(EPSILON, &token_end, &token_start);
            }
            Meta::ZeroOrMore => {
                add_transition(EPSILON, &start, &end);
                let (token_start, token_end) = token_to_nfa(token);
                add_transition(EPSILON, &start, &token_start);
                add_transition(EPSILON, &token_end, &end);
                add_transition(EPSILON, &token_end, &token_start);
            }
            Meta::ExactCount(count) => {
                let mut end_prev = start.clone();
                for _ in 0..*count {
                    let (start_next, end_next) = token_to_nfa(token);
                    add_transition(EPSILON, &end_prev, &start_next);
                    end_prev = end_next;
                }
                add_transition(EPSILON, &end_prev, &end);
            }
            _ => unreachable!(),
        },
        // Create a special transition that validates the backreference
        Token::BackRef(index) => {
            let backref_char = char::from_u32(BACKREF_BASE as u32 + *index as u32).unwrap();
            add_transition(backref_char, &start, &end);
        }
    }
    (start, end)
}

// Simulates the ε-NFA on the input text
pub fn simulate_nfa(
    state: StatePtr,
    text: &[char],
    pos: usize,
    context: &mut MatchContext,
) -> bool {
    let state_ref = state.borrow();

    if state_ref.terminal {
        return true;
    }

    let ch = {
        if pos == text.len() {
            END_OF_TEXT
        } else {
            text[pos]
        }
    };

    for (transition_char, next_states) in &state_ref.transitions {
        match *transition_char {
            // Handle capture group start
            c if is_capture_start(c) => {
                let capture_index = get_capture_index(c, CAPTURE_START_BASE);
                context.active_captures.insert(capture_index, pos);
                for next_state in next_states {
                    if simulate_nfa(Rc::clone(next_state), text, pos, context) {
                        return true;
                    }
                }
            }

            // Handle capture group end
            c if is_capture_end(c) => {
                let capture_index = get_capture_index(c, CAPTURE_END_BASE);
                if let Some(start_pos) = context.active_captures.get(&capture_index) {
                    let captured_text: String = text[*start_pos..pos].iter().collect();
                    context.captures.insert(capture_index, captured_text);
                }
                for next_state in next_states {
                    if simulate_nfa(Rc::clone(next_state), text, pos, context) {
                        return true;
                    }
                }
            }

            // Handle backreferences
            c if is_backreference(c) => {
                let backref_index = get_backref_index(c);
                if let Some(captured_text) = context.captures.get(&backref_index) {
                    let captured_chars: Vec<char> = captured_text.chars().collect();
                    if pos + captured_chars.len() <= text.len()
                        && text[pos..pos + captured_chars.len()] == captured_chars
                    {
                        for next_state in next_states {
                            if simulate_nfa(
                                Rc::clone(next_state),
                                text,
                                pos + captured_chars.len(),
                                context,
                            ) {
                                return true;
                            }
                        }
                    }
                }
            }

            // Regular character transitions (includes ^ and $)
            c if c == ch => {
                for next_state in next_states {
                    if simulate_nfa(Rc::clone(next_state), text, pos + 1, context) {
                        return true;
                    }
                }
            }

            // Any character transitions (excludes ^ and $)
            c if c == ANY && ch != '^' && ch != '$' => {
                if !has_deadend_transition(&state_ref, ch) {
                    for next_state in next_states {
                        if simulate_nfa(Rc::clone(next_state), text, pos + 1, context) {
                            return true;
                        }
                    }
                }
            }

            // Epsilon transitions (consumes no chars)
            c if c == EPSILON => {
                for next_state in next_states {
                    if simulate_nfa(Rc::clone(next_state), text, pos, context) {
                        return true;
                    }
                }
            }

            _ => {} // Ignore other transitions
        }
    }

    false
}

// Helpers

fn add_transition(c: char, start: &Rc<RefCell<State>>, end: &Rc<RefCell<State>>) {
    start
        .borrow_mut()
        .transitions
        .entry(c)
        .or_default()
        .push(Rc::clone(end));
}

fn is_capture_start(c: char) -> bool {
    c >= CAPTURE_START_BASE && c < char::from_u32(CAPTURE_START_BASE as u32 + 100).unwrap()
}

fn is_capture_end(c: char) -> bool {
    c >= CAPTURE_END_BASE && c < char::from_u32(CAPTURE_END_BASE as u32 + 100).unwrap()
}

fn is_backreference(c: char) -> bool {
    ('\u{F000}'..'\u{F100}').contains(&c)
}

fn get_capture_index(c: char, base: char) -> usize {
    (c as u32 - base as u32) as usize
}

fn get_backref_index(c: char) -> usize {
    (c as u32 - 0xF000) as usize
}

fn has_deadend_transition(state_ref: &std::cell::Ref<State>, ch: char) -> bool {
    state_ref.transitions.get(&ch).is_some_and(|states| {
        states.iter().any(|s| {
            let s_ref = s.borrow();
            !s_ref.initial && !s_ref.terminal && s_ref.transitions.is_empty()
        })
    })
}

