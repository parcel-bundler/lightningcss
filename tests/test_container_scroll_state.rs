use lightningcss::stylesheet::{ParserOptions, StyleSheet};

#[test]
fn test_container_scroll_state_query() {
    let css = r#"
        @container scroll-state(scrollable: left) {
            .foo { color: red; }
        }
    "#;
    let stylesheet = StyleSheet::parse(css, ParserOptions::default()).unwrap();
    let css_out = stylesheet.to_css(Default::default()).unwrap().code;
    assert!(css_out.contains("@container scroll-state(scrollable: left)"));
}

#[test]
fn test_container_scroll_state_stuck_top() {
    let css = r#"
        @container scroll-state(stuck: top) {
            .bar { color: blue; }
        }
    "#;
    let stylesheet = StyleSheet::parse(css, ParserOptions::default()).unwrap();
    let css_out = stylesheet.to_css(Default::default()).unwrap().code;
    assert!(css_out.contains("@container scroll-state(stuck: top)"));
}