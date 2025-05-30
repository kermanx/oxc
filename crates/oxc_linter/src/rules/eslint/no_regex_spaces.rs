use oxc_allocator::{Allocator, Vec};
use oxc_ast::{
    AstKind,
    ast::{Argument, CallExpression, NewExpression, RegExpLiteral},
};
use oxc_diagnostics::OxcDiagnostic;
use oxc_macros::declare_oxc_lint;
use oxc_regular_expression::{
    ConstructorParser, Options,
    ast::{Character, Pattern},
    visit::{RegExpAstKind, Visit},
};
use oxc_span::Span;

use crate::{AstNode, context::LintContext, rule::Rule};

fn no_regex_spaces_diagnostic(span: Span) -> OxcDiagnostic {
    OxcDiagnostic::warn("Multiple consecutive spaces are hard to count.")
        .with_help(format!("Use a quantifier: ` {{{size}}}`", size = span.size()))
        .with_label(span)
}

#[derive(Debug, Default, Clone)]
pub struct NoRegexSpaces;

declare_oxc_lint!(
    /// ### What it does
    ///
    /// Disallow 2+ consecutive spaces in regular expressions.
    ///
    /// ### Why is this bad?
    ///
    /// In a regular expression, it is hard to tell how many spaces are
    /// intended to be matched. It is better to use only one space and
    /// then specify how many spaces are expected using a quantifier.
    ///
    /// ```javascript
    /// var re = /foo {3}bar/;
    /// ```
    ///
    /// ### Examples
    ///
    /// Examples of **incorrect** code for this rule:
    /// ```javascript
    /// var re = /foo   bar/;
    /// ```
    NoRegexSpaces,
    eslint,
    restriction,
    pending // TODO: This is somewhat autofixable, but the fixer does not exist yet.
);

impl Rule for NoRegexSpaces {
    fn run<'a>(&self, node: &AstNode<'a>, ctx: &LintContext<'a>) {
        match node.kind() {
            AstKind::RegExpLiteral(lit) => {
                if let Some(span) = Self::find_literal_to_report(lit) {
                    ctx.diagnostic(no_regex_spaces_diagnostic(span)); // /a  b/
                }
            }

            AstKind::CallExpression(expr) if Self::is_regexp_call_expression(expr) => {
                if let Some(span) = Self::find_expr_to_report(&expr.arguments, ctx) {
                    ctx.diagnostic(no_regex_spaces_diagnostic(span)); // RegExp('a  b')
                }
            }

            AstKind::NewExpression(expr) if Self::is_regexp_new_expression(expr) => {
                if let Some(span) = Self::find_expr_to_report(&expr.arguments, ctx) {
                    ctx.diagnostic(no_regex_spaces_diagnostic(span)); // new RegExp('a  b')
                }
            }
            _ => {}
        }
    }
}

impl NoRegexSpaces {
    fn find_literal_to_report(literal: &RegExpLiteral) -> Option<Span> {
        let pattern_text = literal.regex.pattern.text.as_str();
        if !Self::has_double_space(pattern_text) {
            return None;
        }

        let pattern = literal.regex.pattern.pattern.as_deref()?;
        find_consecutive_spaces(pattern)
    }

    fn find_expr_to_report(args: &Vec<'_, Argument<'_>>, ctx: &LintContext) -> Option<Span> {
        if let Some(expr) = args.get(1).and_then(Argument::as_expression) {
            if !expr.is_string_literal() {
                return None; // skip on indeterminate flag, e.g. RegExp('a  b', flags)
            }
        }

        let Some(Argument::StringLiteral(pattern)) = args.first() else {
            return None;
        };
        if !Self::has_double_space(&pattern.value) {
            return None;
        }

        let alloc = Allocator::default();
        let parser = ConstructorParser::new(
            &alloc,
            pattern.span.source_text(ctx.source_text()),
            None,
            Options { pattern_span_offset: pattern.span.start, ..Options::default() },
        );
        let parsed_pattern = parser.parse().ok()?;

        find_consecutive_spaces(&parsed_pattern)
    }

    fn is_regexp_new_expression(expr: &NewExpression<'_>) -> bool {
        expr.callee.is_specific_id("RegExp") && !expr.arguments.is_empty()
    }

    fn is_regexp_call_expression(expr: &CallExpression<'_>) -> bool {
        expr.callee.is_specific_id("RegExp") && !expr.arguments.is_empty()
    }

    // For skipping if there aren't any consecutive spaces in the source, to avoid reporting cases
    // where the space is explicitly escaped, like: `RegExp(' \ ')``.
    fn has_double_space(input: &str) -> bool {
        input.contains("  ")
    }
}

fn find_consecutive_spaces(pattern: &Pattern) -> Option<Span> {
    let mut finder = ConsecutiveSpaceFinder { last_space_span: None, depth: 0 };
    finder.visit_pattern(pattern);

    // return none if span is only one space
    finder.last_space_span.filter(|span| span.size() > 1)
}

struct ConsecutiveSpaceFinder {
    last_space_span: Option<Span>,
    depth: u32,
}

impl<'a> Visit<'a> for ConsecutiveSpaceFinder {
    fn enter_node(&mut self, kind: RegExpAstKind<'a>) {
        if let RegExpAstKind::Quantifier(_) | RegExpAstKind::CharacterClass(_) = kind {
            self.depth += 1;
        }
    }

    fn leave_node(&mut self, kind: RegExpAstKind<'a>) {
        if let RegExpAstKind::Quantifier(_) | RegExpAstKind::CharacterClass(_) = kind {
            self.depth -= 1;
        }
    }

    fn visit_character(&mut self, ch: &Character) {
        if self.depth > 0 {
            return;
        }
        if ch.value != u32::from(b' ') {
            return;
        }
        if let Some(space_span) = &mut self.last_space_span {
            // If this is consecutive with the last space, extend it
            if space_span.end == ch.span.start {
                space_span.end = ch.span.end;
            }
            // If it is not consecutive, and the last space is only one space, move it up
            else if space_span.size() == 1 {
                self.last_space_span.replace(ch.span);
            }
        } else {
            self.last_space_span = Some(ch.span);
        }
    }
}

#[test]
fn test() {
    use crate::tester::Tester;

    let pass = vec![
        "var foo = /foo/;",
        "var foo = RegExp('foo')",
        "var foo = / /;",
        "var foo = RegExp(' ')",
        "var foo = / a b c d /;",
        "var foo = /bar {3}baz/g;",
        "var foo = RegExp('bar {3}baz', 'g')",
        "var foo = new RegExp('bar {3}baz')",
        "var foo = /bar			baz/;",
        "var foo = RegExp('bar			baz');",
        "var foo = new RegExp('bar			baz');",
        "var foo = /  +/;",
        "var foo = /  ?/;",
        "var foo = /  */;",
        "var foo = /  {2}/;",
        "var foo = /  {2}/v;",
        "var foo = / /;",
        r"var foo = /bar \\ baz/;",
        r"var foo = /bar\\ \\ baz/;",
        r"var foo = /bar \\u0020 baz/;",
        r"var foo = /bar \\u0020\\u0020baz/;",
        r"var foo = new RegExp('bar \\ baz')",
        r"var foo = new RegExp('bar\\ \\ baz')",
        r"var foo = new RegExp('bar \\\\ baz')",
        r"var foo = new RegExp('bar\\u0020\\u0020baz')",
        r"var foo = new RegExp('bar \\u0020 baz')",
        "new RegExp('  ', flags)",
        "var foo = /[  ]/;",
        "var foo = /[   ]/;",
        "var foo = / [  ] /;",
        "var foo = / [  ] [  ] /;",
        "var foo = new RegExp('[  ]');",
        "var foo = new RegExp('[   ]');",
        "var foo = new RegExp(' [  ] ');",
        "var foo = RegExp(' [  ] [  ] ');",
        r"var foo = new RegExp(' \[   ');",
        r"var foo = new RegExp(' \[   \] ');",
        "var foo = /[\\q{    }]/v;",
        "var foo = new RegExp('[  ');",
        "new RegExp('[[abc]  ]', flags + 'v')",
    ];

    let fail = vec![
        "var foo = /  /;",
        "var foo = /bar  baz/;",
        "var foo = /bar    baz/;",
        "var foo = / a b  c d /;",
        "var foo = RegExp(' a b c d  ');",
        "var foo = RegExp('bar    baz');",
        "var foo = new RegExp('bar    baz');",
        "var foo = /bar   {3}baz/;",
        "var foo = /bar    ?baz/;",
        "var foo = new RegExp('bar   *baz')",
        "var foo = RegExp('bar   +baz')",
        "var foo = new RegExp('bar    ');",
        r"var foo = /bar\\  baz/;",
        "var foo = /(?:  )/;",
        "var foo = RegExp('^foo(?=   )');",
        r"var foo = /\\  /",
        r"var foo = / \\  /",
        "var foo = /  foo   /;",
        r"var foo = new RegExp('\\d  ')",
        r"var foo = RegExp('\\u0041   ')",
        "var foo = /[   ]  /;",
        "var foo = /  [   ] /;",
        "var foo = RegExp('  [ ]');",
        "var foo = /[[    ]    ]    /v;",
        "var foo = new RegExp('[   ]  ');",
        "var foo = new RegExp('[[    ]    ]    ', 'v');",
    ];

    Tester::new(NoRegexSpaces::NAME, NoRegexSpaces::PLUGIN, pass, fail).test_and_snapshot();
}
