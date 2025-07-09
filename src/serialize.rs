pub use cssparser::{serialize_identifier as identifier, serialize_name as name};

/// Write a quoted CSS string token, escaping content as necessary.
///
/// Uses single or double quotes depending on the presence of quote characters
/// in the `value`.
pub fn string(value: &str, dest: &mut impl core::fmt::Write) -> core::fmt::Result {
    let count = value.bytes().fold(0isize, |count, ch| {
        match ch {
          b'"'  => count + 1,
          b'\'' => count - 1,
          _     => count,
    }});
    let (quote, quote_str, escaped_quote) = if count <= 0 {
        (b'"', "\"", "\\\"")
    } else {
        (b'\'', "'", "\\'")
    };

    // Based on cssparser::serialize_string but with less indirection and
    // generic code, and added handling for custom quote character.
    dest.write_str(quote_str)?;
    let bytes = value.as_bytes();
    let mut chunk_start = 0;
    for (i, byte) in bytes.iter().copied().enumerate() {
        let mut buf = [0; 4];
        let escaped = cssparser_macros::match_byte! { byte,
            b'"' | b'\'' => if byte == quote {
                escaped_quote
            } else {
                continue
            },
            b'\\' => "\\\\",
            b'\0' => "\u{FFFD}",
            b'\x01'..=b'\x1F' | b'\x7F' => hex_escape(byte, &mut buf, bytes.get(i + 1)),
            _ => continue,
        };
        dest.write_str(&value[chunk_start..i])?;
        dest.write_str(escaped)?;
        chunk_start = i + 1;
    }
    dest.write_str(&value[chunk_start..])?;
    dest.write_str(quote_str)
}


/// Escapes an ASCII character using hex encoding.
///
/// `next` indicates character in the string that followers the byte we’re
/// escaping.  If it’s a hexadecimal digit, escaped representation will be
/// followed by a space.  Otherwise (including if `next` is `None`), no space
/// will be included.
// Based on cssparser::hex_escape
fn hex_escape<'a>(byte: u8, buf: &'a mut [u8; 4], next: Option<&u8>) -> &'a str {
    static HEX_DIGITS: &[u8; 16] = b"0123456789abcdef";
    let mut len = if byte <= 0x0F {
        buf[0] = b'\\';
        buf[1] = HEX_DIGITS[usize::from(byte)];
        buf[2] = b' ';
        buf[3] = b' ';
        2
    } else {
        buf[0] = b'\\';
        buf[1] = HEX_DIGITS[usize::from(byte >> 4)];
        buf[2] = HEX_DIGITS[usize::from(byte & 0xF)];
        buf[3] = b' ';
        3
    };
    if next.map_or(false, u8::is_ascii_hexdigit) {
        len += 1;
    };
    // SAFETY: We’ve written only ASCII characters to `buf`.
    unsafe { core::str::from_utf8_unchecked(&buf[..len]) }
}

#[test]
fn test_string() {
    for (value, quoted) in [
        ("foo", "\"foo\""),
        (r#""Hejże," he exclaimed."#, r#"'"Hejże," he exclaimed.'"#),
        ("rock'n'roll", r#""rock'n'roll""#),
        (r#""I love Rock'n'Roll," she shouted."#,
         r#""\"I love Rock'n'Roll,\" she shouted.""#),

        ("\x01",  r#""\1""#),
        ("\x01A", r#""\1 A""#),
        ("\x01Z", r#""\1Z""#),
    ] {
        let mut got = String::new();
        string(value, &mut got).unwrap();
        assert_eq!(quoted, got);
    }
}
