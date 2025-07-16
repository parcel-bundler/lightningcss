use lightningcss::properties::{Property, PropertyId};
use lightningcss::declaration::DeclarationBlock;
use lightningcss::stylesheet::ParserOptions;
use std::borrow::Cow;

#[test]
fn test_container_type_scroll_state() {
    let decls = DeclarationBlock::parse_string(
        "container-type: scroll-state;",
        ParserOptions::default(),
    )
    .unwrap();
    let v = decls.get(&PropertyId::ContainerType);
    assert!(matches!(v, Some((cow, _)) if matches!(cow.as_ref(), Property::ContainerType(lightningcss::properties::contain::ContainerType::ScrollState))));
}
