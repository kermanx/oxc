use oxc_ast::{
    AstKind,
    ast::{JSXAttributeItem, JSXAttributeName},
};
use oxc_diagnostics::OxcDiagnostic;
use oxc_macros::declare_oxc_lint;
use oxc_span::{Atom, Span};
use rustc_hash::FxHashMap;

use crate::{
    AstNode,
    context::{ContextHost, LintContext},
    rule::Rule,
};

fn jsx_no_duplicate_props_diagnostic(prop_name: &str, span1: Span, span2: Span) -> OxcDiagnostic {
    OxcDiagnostic::warn(format!(
        "No duplicate props allowed. The prop \"{prop_name}\" is duplicated."
    ))
    .with_help("Remove one of the props, or rename them so each prop is distinct.")
    .with_labels([span1, span2])
}

#[derive(Debug, Default, Clone)]
pub struct JsxNoDuplicateProps;

declare_oxc_lint!(
    /// ### What it does
    ///
    /// This rule prevents duplicate props in JSX elements.
    ///
    /// ### Why is this bad?
    ///
    /// Having duplicate props in a JSX element is most likely a mistake.
    /// Creating JSX elements with duplicate props can cause unexpected behavior in your application.
    ///
    /// ### Examples
    ///
    /// Examples of **incorrect** code for this rule:
    /// ```jsx
    /// <App a a />;
    /// <App foo={2} bar baz foo={3} />;
    /// ```
    ///
    /// Examples of **correct** code for this rule:
    /// ```jsx
    /// <App a />;
    /// <App bar baz foo={3} />;
    /// ```
    JsxNoDuplicateProps,
    react,
    correctness
);

impl Rule for JsxNoDuplicateProps {
    fn run<'a>(&self, node: &AstNode<'a>, ctx: &LintContext<'a>) {
        let AstKind::JSXOpeningElement(jsx_opening_elem) = node.kind() else {
            return;
        };

        let mut props: FxHashMap<Atom, Span> = FxHashMap::default();

        for attr in &jsx_opening_elem.attributes {
            let JSXAttributeItem::Attribute(jsx_attr) = attr else {
                continue;
            };

            let JSXAttributeName::Identifier(ident) = &jsx_attr.name else {
                continue;
            };

            if let Some(old_span) = props.insert(ident.name, ident.span) {
                ctx.diagnostic(jsx_no_duplicate_props_diagnostic(
                    ident.name.as_str(),
                    old_span,
                    ident.span,
                ));
            }
        }
    }

    fn should_run(&self, ctx: &ContextHost) -> bool {
        ctx.source_type().is_jsx()
    }
}

#[test]
fn test() {
    use crate::tester::Tester;

    let pass = vec![
        ("<App />;", None),
        ("<App {...this.props} />;", None),
        ("<App a b c />;", None),
        ("<App a b c A />;", None),
        ("<App {...this.props} a b c />;", None),
        ("<App c {...this.props} a b />;", None),
        (r#"<App a="c" b="b" c="a" />;"#, None),
        (r#"<App {...this.props} a="c" b="b" c="a" />;"#, None),
        (r#"<App c="a" {...this.props} a="c" b="b" />;"#, None),
        ("<App A a />;", None),
        ("<App A b a />;", None),
        (r#"<App A="a" b="b" B="B" />;"#, None),
    ];

    let fail = vec![
        ("<App a a />;", None),
        ("<App A b c A />;", None),
        (r#"<App a="a" b="b" a="a" />;"#, None),
        (r#"<App a="a" {...this.props} b="b" a="a" />;"#, None),
        (r#"<App a b="b" {...this.props} a="a" />;"#, None),
        (r#"<App a={[]} b="b" {...this.props} a="a" />;"#, None),
        (r#"<App a="a" b="b" a="a" {...this.props} />;"#, None),
        (r#"<App {...this.props} a="a" b="b" a="a" />;"#, None),
        (
            r#"
            <App
                a="a"
                {...this.props}
                a={{foo: 'bar'}}
                b="b"
            />;
        "#,
            None,
        ),
    ];

    Tester::new(JsxNoDuplicateProps::NAME, JsxNoDuplicateProps::PLUGIN, pass, fail)
        .test_and_snapshot();
}
