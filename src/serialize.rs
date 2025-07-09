pub use cssparser::{serialize_identifier as identifier, serialize_name as name};

static HEX_DIGITS: [u8; 16] = *b"0123456789abcdef";

/// Write a quoted CSS string token, escaping content as necessary.
///
/// Uses single or double quotes depending on the presence of quote characters
/// in the `value`.
pub fn string(value: &str, dest: &mut impl core::fmt::Write) -> core::fmt::Result {
  let count: isize = value
    .bytes()
    .map(|ch| match ch {
      b'"' => 1,
      b'\'' => -1,
      _ => 0,
    })
    .sum();
  let (quote, quote_str, escaped_quote) = if count <= 0 {
    (b'"', "\"", "\\\"")
  } else {
    (b'\'', "'", "\\'")
  };

  // Based on cssparser::serialize_string.
  dest.write_str(quote_str)?;
  let mut chunk_start = 0;
  let mut buf = [b'\\', b' ', b' ', b' '];
  for (i, byte) in value.bytes().enumerate() {
    let escaped = if byte == quote {
      escaped_quote
    } else if byte == b'\\' {
        "\\\\"
    } else if byte == 0 {
        "\u{FFFD}"  // Replacement character
    } else if matches!(byte, 1..=31 | 0x7F) {
      // CSS normalises FF, CR and LF hence those have to be escaped, see
      // <https://www.w3.org/TR/css-syntax-3/#input-preprocessing>.
      //
      // CSS allows other control characters, but we need to worry about styles
      // embedded in other documents, most notably XML.  It disallows control
      // characters except for tab, LF and CR, which adds all but tab character
      // to the set of characters we need to escape, see
      // <https://www.w3.org/TR/xml/#charsets>.
      //
      // However, XML normalises attribute values by replacing white-space
      // characters by a space, see <https://www.w3.org/TR/xml/#AVNormalize>.
      // This means, that we also need to escape tab character.
      //
      // Also escape DEL byte, even though CSS, XML and HTML allow it.  This is
      // what CSSOM does and we follow suit, see
      // <https://drafts.csswg.org/cssom-1/#serialize-a-string>.
      let len = if byte < 16 {
        buf[1] = HEX_DIGITS[usize::from(byte)];
        buf[2] = b' ';
        2
      } else {
        buf[1] = HEX_DIGITS[usize::from(byte / 16)];
        buf[2] = HEX_DIGITS[usize::from(byte % 16)];
        3
      };
      let next = value.as_bytes().get(i + 1);
      let len = len + usize::from(problematic_after_escape(next));
      // TODO: Use core::ascii::Char once it stabilises.
      core::str::from_utf8(&buf[..len]).unwrap()
    } else {
      continue;
    };
    dest.write_str(&value[chunk_start..i])?;
    dest.write_str(escaped)?;
    chunk_start = i + 1;
  }
  dest.write_str(&value[chunk_start..])?;
  dest.write_str(quote_str)
}

/// Returns whether given character would be consumed as part of escaped code
/// point if it followed the escape; except, since we always escape tab and
/// newline characters, these are treated as not consumed.
///
/// In other words, returns whether the given character is a hexadecimal digit,
/// a space character.
///
/// See <https://www.w3.org/TR/css-syntax-3/#consume-an-escaped-code-point>.
fn problematic_after_escape(ch: Option<&u8>) -> bool {
  ch.is_some_and(|&ch| ch.is_ascii_hexdigit() || ch == b' ')
}

#[test]
fn test_string() {
  for (value, quoted) in [
    // Base case.
    ("foo", "\"foo\""),
    // Pick best quote character.
    (r#""Hejże," he exclaimed."#, r#"'"Hejże," he exclaimed.'"#),
    ("rock'n'roll", r#""rock'n'roll""#),
    (
      r#""I love Rock'n'Roll," she shouted."#,
      r#""\"I love Rock'n'Roll,\" she shouted.""#,
    ),
    // Escaping the newline character and space afterwards.
    ("\n", r#""\a""#),
    ("\nA", r#""\a A""#),
    ("\nZ", r#""\aZ""#),
    ("\n ", r#""\a  ""#),
    ("\n\t", r#""\a\9""#),
    ("\n\n", r#""\a\a""#),
    // Escaping other control characters and DEL.
    ("\x00A", "\"\u{FFFD}A\""),
    ("\x01A", r#""\1 A""#),
    ("\x0AA", r#""\a A""#),
    ("\x0CA", r#""\c A""#),
    ("\x0DA", r#""\d A""#),
    ("\x7FA", r#""\7f A""#),
  ] {
    let mut got = String::new();
    string(value, &mut got).unwrap();
    assert_eq!(quoted, got, "input: {value:?}");
  }
}
