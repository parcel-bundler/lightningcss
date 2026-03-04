use crate::error::{Error, ErrorLocation};

pub(crate) fn format_source_location_context(source: &str, loc: &ErrorLocation) -> String {
  let lines: Vec<&str> = source.lines().collect();
  let line_idx = loc.line as usize;
  let display_line = line_idx + 1;
  let display_column = if loc.column == 0 { 1 } else { loc.column as usize };

  let mut output = format!("css location: line {display_line}, column {display_column}");

  if lines.is_empty() {
    output.push_str("\n  (source is empty)");
    return output;
  }

  if line_idx >= lines.len() {
    output.push_str(&format!("\n  (line is out of range; source has {} line(s))", lines.len()));
    return output;
  }

  let start = line_idx.saturating_sub(1);
  let end = usize::min(line_idx + 1, lines.len() - 1);
  output.push_str("\ncontext:");

  for i in start..=end {
    let line = lines[i];
    output.push_str(&format!("\n{:>6} | {}", i + 1, line));
    if i == line_idx {
      let caret_pos = display_column.saturating_sub(1);
      let line_char_count = line.chars().count();
      let mut marker = String::with_capacity(caret_pos.max(line_char_count));
      for ch in line.chars().take(caret_pos) {
        marker.push(if ch == '\t' { '\t' } else { ' ' });
      }
      if caret_pos > line_char_count {
        marker.push_str(&" ".repeat(caret_pos - line_char_count));
      }
      output.push_str(&format!("\n       | {}^", marker));
    }
  }

  output
}

#[track_caller]
pub(crate) fn panic_with_test_error<T: std::fmt::Debug + std::fmt::Display>(
  helper: &str,
  stage: &str,
  source: &str,
  error: Error<T>,
) -> ! {
  let caller = std::panic::Location::caller();
  let location = match error.loc.as_ref() {
    Some(loc) => format_source_location_context(source, loc),
    None => "css location: <none>".to_string(),
  };
  panic!(
    "{helper}: {stage} failed\ncaller: {}:{}\nerror: {error}\nerror(debug): {error:?}\n{location}\nsource:\n{source}",
    caller.file(),
    caller.line(),
  );
}
