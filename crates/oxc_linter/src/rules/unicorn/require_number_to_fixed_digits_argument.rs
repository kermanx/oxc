use oxc_ast::{AstKind, ast::Expression};
use oxc_diagnostics::OxcDiagnostic;
use oxc_macros::declare_oxc_lint;
use oxc_span::{GetSpan, Span};

use crate::{AstNode, context::LintContext, rule::Rule};

fn require_number_to_fixed_digits_argument_diagnostic(span: Span) -> OxcDiagnostic {
    OxcDiagnostic::warn("Number method .toFixed() should have an argument")
        .with_help("It's better to make it clear what the value of the digits argument is when calling Number#toFixed(), instead of relying on the default value of 0.")
        .with_label(span)
}

#[derive(Debug, Default, Clone)]
pub struct RequireNumberToFixedDigitsArgument;

declare_oxc_lint!(
    /// ### What it does
    ///
    /// Enforce using the digits argument with Number.toFixed()
    ///
    /// ### Why is this bad?
    ///
    /// It's better to make it clear what the value of the digits argument is when calling Number.toFixed(),
    /// instead of relying on the default value of 0.
    ///
    /// ### Examples
    ///
    /// Examples of **incorrect** code for this rule:
    /// ```javascript
    /// number.toFixed();
    /// ```
    ///
    /// Examples of **correct** code for this rule:
    /// ```javascript
    /// number.toFixed(0);
    /// number.toFixed(2);
    /// ```
    RequireNumberToFixedDigitsArgument,
    unicorn,
    pedantic,
    fix
);

impl Rule for RequireNumberToFixedDigitsArgument {
    fn run<'a>(&self, node: &AstNode<'a>, ctx: &LintContext<'a>) {
        let AstKind::CallExpression(expr) = node.kind() else {
            return;
        };

        if !expr.arguments.is_empty() || expr.optional {
            return;
        }

        if let Some(member) = expr.callee.get_member_expr() {
            if let Expression::NewExpression(_) = member.object() {
                return;
            }

            if member.optional() || member.is_computed() {
                return;
            }

            if let Some(property_name) = member.static_property_name() {
                if property_name == "toFixed" {
                    let parenthesis_span = Span::new(member.span().end, expr.span.end);

                    ctx.diagnostic_with_fix(
                        require_number_to_fixed_digits_argument_diagnostic(parenthesis_span),
                        |fixer| {
                            let modified_code = {
                                let span_source_code = fixer.source_range(Span::new(
                                    parenthesis_span.start,
                                    parenthesis_span.end - 1,
                                ));

                                let mut code = String::with_capacity(span_source_code.len() + 2);
                                code.push_str(span_source_code);
                                code.push_str("0)");
                                code
                            };

                            fixer.replace(parenthesis_span, modified_code)
                        },
                    );
                }
            }
        }
    }
}

#[test]
fn test() {
    use crate::tester::Tester;

    let pass = vec![
        "number.toFixed(0)",
        "number.toFixed(...[])",
        "number.toFixed(2)",
        "number.toFixed(1,2,3)",
        "number[toFixed]()",
        "number[\"toFixed\"]()",
        "number?.toFixed()",
        "number.toFixed?.()",
        "number.notToFixed();",
        "new BigNumber(1).toFixed()",
        "new Number(1).toFixed()",
    ];

    let fail = vec![
        "const string = number.toFixed();",
        "const string = number.toFixed( /* comment */ );",
        "Number(1).toFixed()",
        "const bigNumber = new BigNumber(1); const string = bigNumber.toFixed();",
    ];

    let fix = vec![
        ("const string = number.toFixed();", "const string = number.toFixed(0);"),
        (
            "const string = number.toFixed( /* comment */ );",
            "const string = number.toFixed( /* comment */ 0);",
        ),
        ("Number(1).toFixed()", "Number(1).toFixed(0)"),
        (
            "const bigNumber = new BigNumber(1); const string = bigNumber.toFixed();",
            "const bigNumber = new BigNumber(1); const string = bigNumber.toFixed(0);",
        ),
    ];

    Tester::new(
        RequireNumberToFixedDigitsArgument::NAME,
        RequireNumberToFixedDigitsArgument::PLUGIN,
        pass,
        fail,
    )
    .expect_fix(fix)
    .test_and_snapshot();
}
