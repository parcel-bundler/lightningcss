use criterion::black_box;
use lightningcss::stylesheet::{MinifyOptions, ParserOptions, PrinterOptions, StyleSheet};
use std::fmt::Write;

pub struct Fixture {
  pub name: &'static str,
  pub css: String,
}

pub fn stylesheet_fixtures() -> Vec<Fixture> {
  vec![
    Fixture {
      name: "small",
      css: small_fixture(),
    },
    Fixture {
      name: "medium",
      css: generated_fixture(48, 96),
    },
    Fixture {
      name: "large",
      css: generated_fixture(192, 384),
    },
  ]
}

pub fn parse_stylesheet(css: &str) {
  let stylesheet = StyleSheet::parse(css, ParserOptions::default()).unwrap();
  black_box(stylesheet);
}

pub fn transform_stylesheet(css: &str) {
  let mut stylesheet = StyleSheet::parse(css, ParserOptions::default()).unwrap();
  stylesheet.minify(MinifyOptions::default()).unwrap();
  let result = stylesheet
    .to_css(PrinterOptions {
      minify: true,
      ..PrinterOptions::default()
    })
    .unwrap();
  black_box(result);
}

fn small_fixture() -> String {
  r#"
:root {
  --brand: #3366ff;
  --surface: color-mix(in srgb, white 88%, var(--brand));
}

.button {
  appearance: none;
  background: linear-gradient(135deg, var(--brand), #0bb3ff);
  border: 1px solid color-mix(in srgb, var(--brand), black 18%);
  border-radius: 8px;
  color: white;
  display: inline-flex;
  gap: 0.5rem;
  padding: 0.625rem 1rem;
  transition: transform 180ms ease, box-shadow 180ms ease;
}

@media (hover: hover) {
  .button:hover {
    box-shadow: 0 12px 32px rgb(18 52 99 / 22%);
    transform: translateY(-1px);
  }
}
"#
  .to_owned()
}

fn generated_fixture(component_count: usize, utility_count: usize) -> String {
  let mut css = String::with_capacity(component_count * 920 + utility_count * 96);

  css.push_str(
    r#"
@layer reset, components, utilities;

@layer reset {
  *,
  ::before,
  ::after {
    box-sizing: border-box;
  }

  body {
    color: #162033;
    font-family: ui-sans-serif, system-ui, sans-serif;
    margin: 0;
  }
}
"#,
  );

  for i in 0..component_count {
    let hue = (i * 19) % 360;
    let next_hue = (hue + 36) % 360;
    let columns = (i % 4) + 2;
    let gap = (i % 8) + 8;
    let width = 36 + (i % 28);
    let padding = 12 + (i % 10);

    writeln!(
      css,
      r#"
@layer components {{
  .card-{i},
  .dashboard-{i} > .panel {{
    background:
      linear-gradient(135deg, hsl({hue}deg 64% 96%), hsl({next_hue}deg 72% 92%)),
      white;
    border: 1px solid hsl({hue}deg 28% 82%);
    border-radius: {radius}px;
    box-shadow: 0 {shadow_y}px {shadow_blur}px rgb(20 31 48 / 12%);
    color: hsl({hue}deg 45% 22%);
    display: grid;
    gap: {gap}px;
    grid-template-columns: minmax(0, 1fr) auto;
    padding: {padding}px;
  }}

  .card-{i}:where(:hover, :focus-visible) {{
    border-color: hsl({hue}deg 72% 48%);
    box-shadow: 0 {hover_y}px {hover_blur}px rgb(20 31 48 / 18%);
    transform: translateY(-1px);
  }}

  .card-{i} .title {{
    font-size: clamp(1rem, 0.92rem + 0.3vw, 1.25rem);
    font-weight: 700;
    letter-spacing: 0;
    margin: 0;
  }}

  .card-{i} .meta {{
    color: color-mix(in srgb, hsl({hue}deg 45% 22%), white 42%);
    font-size: 0.875rem;
  }}
}}

@media (min-width: {media_width}px) {{
  .grid-{i} {{
    display: grid;
    gap: {gap}px;
    grid-template-columns: repeat({columns}, minmax(0, 1fr));
  }}
}}

@supports (container-type: inline-size) {{
  .wrap-{i} {{
    container-type: inline-size;
  }}

  @container (min-width: {width}rem) {{
    .wrap-{i} .card-{i} {{
      padding: {container_padding}px;
    }}
  }}
}}
"#,
      radius = 6 + (i % 6),
      shadow_y = 4 + (i % 5),
      shadow_blur = 16 + (i % 12),
      hover_y = 8 + (i % 6),
      hover_blur = 24 + (i % 18),
      media_width = 480 + (i % 9) * 80,
      container_padding = padding + 6
    )
    .unwrap();
  }

  css.push_str("@layer utilities {\n");
  for i in 0..utility_count {
    let value = (i % 32) + 1;
    writeln!(
      css,
      "  .m-{i} {{ margin: {value}px !important; }} .p-{i} {{ padding: {value}px !important; }} .gap-{i} {{ gap: {value}px; }}"
    )
    .unwrap();
  }
  css.push_str("}\n");

  css
}
