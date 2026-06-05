use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use lightningcss::stylesheet::{MinifyOptions, ParserOptions, PrinterOptions, StyleSheet};
use lightningcss::targets::{Browsers, Targets};

const CSS_SMALL: &str = r#"
.button {
  background-color: #4CAF50;
  border: none;
  color: white;
  padding: 15px 32px;
  text-align: center;
  text-decoration: none;
  display: inline-block;
  font-size: 16px;
  margin: 4px 2px;
  cursor: pointer;
  border-radius: 4px;
  transition: background-color 0.3s ease;
}

.button:hover {
  background-color: #45a049;
}
"#;

const CSS_MEDIUM: &str = r#"
:root {
  --primary: #3498db;
  --secondary: #2ecc71;
  --danger: #e74c3c;
  --text: #333333;
  --bg: #ffffff;
  --border: #dddddd;
  --radius: 4px;
  --shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
}

*, *::before, *::after {
  box-sizing: border-box;
  margin: 0;
  padding: 0;
}

body {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen,
    Ubuntu, Cantarell, sans-serif;
  font-size: 16px;
  line-height: 1.5;
  color: var(--text);
  background-color: var(--bg);
}

.container {
  max-width: 1200px;
  margin: 0 auto;
  padding: 0 16px;
}

.grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(280px, 1fr));
  gap: 24px;
}

.card {
  background: var(--bg);
  border: 1px solid var(--border);
  border-radius: var(--radius);
  box-shadow: var(--shadow);
  padding: 24px;
  transition: transform 0.2s ease, box-shadow 0.2s ease;
}

.card:hover {
  transform: translateY(-2px);
  box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
}

.card__title {
  font-size: 1.25rem;
  font-weight: 600;
  margin-bottom: 8px;
  color: var(--text);
}

.card__body {
  font-size: 0.875rem;
  color: #666666;
  margin-bottom: 16px;
}

.btn {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  padding: 8px 16px;
  border: none;
  border-radius: var(--radius);
  font-size: 0.875rem;
  font-weight: 500;
  cursor: pointer;
  transition: background-color 0.2s ease, color 0.2s ease;
  text-decoration: none;
}

.btn--primary {
  background-color: var(--primary);
  color: white;
}

.btn--primary:hover {
  background-color: #2980b9;
}

.btn--secondary {
  background-color: var(--secondary);
  color: white;
}

.btn--danger {
  background-color: var(--danger);
  color: white;
}

.btn--outline {
  background-color: transparent;
  border: 1px solid var(--primary);
  color: var(--primary);
}

.btn--outline:hover {
  background-color: var(--primary);
  color: white;
}

.nav {
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding: 16px 0;
  border-bottom: 1px solid var(--border);
}

.nav__links {
  display: flex;
  gap: 24px;
  list-style: none;
}

.nav__link {
  color: var(--text);
  text-decoration: none;
  font-weight: 500;
  transition: color 0.2s ease;
}

.nav__link:hover {
  color: var(--primary);
}

.form-group {
  margin-bottom: 16px;
}

.form-label {
  display: block;
  font-weight: 500;
  margin-bottom: 4px;
}

.form-input {
  width: 100%;
  padding: 8px 12px;
  border: 1px solid var(--border);
  border-radius: var(--radius);
  font-size: 1rem;
  transition: border-color 0.2s ease;
}

.form-input:focus {
  outline: none;
  border-color: var(--primary);
  box-shadow: 0 0 0 3px rgba(52, 152, 219, 0.2);
}

.table {
  width: 100%;
  border-collapse: collapse;
}

.table th,
.table td {
  padding: 12px 16px;
  text-align: left;
  border-bottom: 1px solid var(--border);
}

.table th {
  font-weight: 600;
  background-color: #f8f9fa;
}

.table tr:hover {
  background-color: #f1f3f5;
}

.badge {
  display: inline-block;
  padding: 2px 8px;
  font-size: 0.75rem;
  font-weight: 600;
  border-radius: 12px;
  background-color: var(--primary);
  color: white;
}

.modal-overlay {
  position: fixed;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  background-color: rgba(0, 0, 0, 0.5);
  display: flex;
  align-items: center;
  justify-content: center;
  z-index: 1000;
}

.modal {
  background: var(--bg);
  border-radius: 8px;
  padding: 32px;
  max-width: 500px;
  width: 100%;
  box-shadow: 0 8px 32px rgba(0, 0, 0, 0.2);
}

@media (max-width: 768px) {
  .grid {
    grid-template-columns: 1fr;
  }

  .nav {
    flex-direction: column;
    gap: 16px;
  }

  .nav__links {
    flex-direction: column;
    gap: 8px;
  }

  .modal {
    margin: 16px;
    padding: 24px;
  }
}

@media (max-width: 480px) {
  body {
    font-size: 14px;
  }

  .container {
    padding: 0 12px;
  }

  .card {
    padding: 16px;
  }
}

@keyframes fadeIn {
  from {
    opacity: 0;
    transform: translateY(10px);
  }
  to {
    opacity: 1;
    transform: translateY(0);
  }
}

@keyframes spin {
  from {
    transform: rotate(0deg);
  }
  to {
    transform: rotate(360deg);
  }
}

.fade-in {
  animation: fadeIn 0.3s ease-out;
}

.spinner {
  width: 24px;
  height: 24px;
  border: 2px solid var(--border);
  border-top-color: var(--primary);
  border-radius: 50%;
  animation: spin 0.8s linear infinite;
}

.sr-only {
  position: absolute;
  width: 1px;
  height: 1px;
  padding: 0;
  margin: -1px;
  overflow: hidden;
  clip: rect(0, 0, 0, 0);
  white-space: nowrap;
  border-width: 0;
}

.flex { display: flex; }
.flex-col { flex-direction: column; }
.items-center { align-items: center; }
.justify-center { justify-content: center; }
.justify-between { justify-content: space-between; }
.gap-1 { gap: 4px; }
.gap-2 { gap: 8px; }
.gap-4 { gap: 16px; }
.gap-6 { gap: 24px; }
.p-2 { padding: 8px; }
.p-4 { padding: 16px; }
.m-2 { margin: 8px; }
.m-4 { margin: 16px; }
.text-sm { font-size: 0.875rem; }
.text-lg { font-size: 1.125rem; }
.text-xl { font-size: 1.25rem; }
.font-bold { font-weight: 700; }
.text-center { text-align: center; }
.rounded { border-radius: var(--radius); }
.rounded-lg { border-radius: 8px; }
.shadow { box-shadow: var(--shadow); }
.w-full { width: 100%; }
.h-full { height: 100%; }
.hidden { display: none; }
.overflow-hidden { overflow: hidden; }
.relative { position: relative; }
.absolute { position: absolute; }
"#;

fn bench_parse(c: &mut Criterion) {
    let mut group = c.benchmark_group("parse");

    group.bench_with_input(BenchmarkId::new("small", "button"), &CSS_SMALL, |b, css| {
        b.iter(|| {
            StyleSheet::parse(css, ParserOptions::default()).unwrap();
        });
    });

    group.bench_with_input(
        BenchmarkId::new("medium", "stylesheet"),
        &CSS_MEDIUM,
        |b, css| {
            b.iter(|| {
                StyleSheet::parse(css, ParserOptions::default()).unwrap();
            });
        },
    );

    group.finish();
}

fn bench_minify(c: &mut Criterion) {
    let mut group = c.benchmark_group("minify");

    group.bench_with_input(BenchmarkId::new("small", "button"), &CSS_SMALL, |b, css| {
        b.iter(|| {
            let mut stylesheet = StyleSheet::parse(css, ParserOptions::default()).unwrap();
            stylesheet.minify(MinifyOptions::default()).unwrap();
        });
    });

    group.bench_with_input(
        BenchmarkId::new("medium", "stylesheet"),
        &CSS_MEDIUM,
        |b, css| {
            b.iter(|| {
                let mut stylesheet = StyleSheet::parse(css, ParserOptions::default()).unwrap();
                stylesheet.minify(MinifyOptions::default()).unwrap();
            });
        },
    );

    group.finish();
}

fn bench_print(c: &mut Criterion) {
    let mut group = c.benchmark_group("print");

    let small_sheet = StyleSheet::parse(CSS_SMALL, ParserOptions::default()).unwrap();
    group.bench_function("small/button", |b| {
        b.iter(|| {
            small_sheet.to_css(PrinterOptions::default()).unwrap();
        });
    });

    let medium_sheet = StyleSheet::parse(CSS_MEDIUM, ParserOptions::default()).unwrap();
    group.bench_function("medium/stylesheet", |b| {
        b.iter(|| {
            medium_sheet.to_css(PrinterOptions::default()).unwrap();
        });
    });

    group.finish();
}

fn bench_minify_and_print(c: &mut Criterion) {
    let mut group = c.benchmark_group("minify_and_print");

    group.bench_with_input(BenchmarkId::new("small", "button"), &CSS_SMALL, |b, css| {
        b.iter(|| {
            let mut stylesheet = StyleSheet::parse(css, ParserOptions::default()).unwrap();
            stylesheet.minify(MinifyOptions::default()).unwrap();
            stylesheet
                .to_css(PrinterOptions {
                    minify: true,
                    ..PrinterOptions::default()
                })
                .unwrap();
        });
    });

    group.bench_with_input(
        BenchmarkId::new("medium", "stylesheet"),
        &CSS_MEDIUM,
        |b, css| {
            b.iter(|| {
                let mut stylesheet = StyleSheet::parse(css, ParserOptions::default()).unwrap();
                stylesheet.minify(MinifyOptions::default()).unwrap();
                stylesheet
                    .to_css(PrinterOptions {
                        minify: true,
                        ..PrinterOptions::default()
                    })
                    .unwrap();
            });
        },
    );

    group.finish();
}

fn bench_prefix(c: &mut Criterion) {
    let targets = Targets {
        browsers: Some(Browsers {
            chrome: Some(30 << 16),
            firefox: Some(30 << 16),
            safari: Some(9 << 16),
            ..Browsers::default()
        }),
        ..Targets::default()
    };

    c.bench_function("prefix/medium", |b| {
        b.iter(|| {
            let mut stylesheet =
                StyleSheet::parse(CSS_MEDIUM, ParserOptions::default()).unwrap();
            stylesheet
                .minify(MinifyOptions {
                    targets,
                    ..MinifyOptions::default()
                })
                .unwrap();
            stylesheet
                .to_css(PrinterOptions {
                    targets,
                    minify: true,
                    ..PrinterOptions::default()
                })
                .unwrap();
        });
    });
}

criterion_group!(
    benches,
    bench_parse,
    bench_minify,
    bench_print,
    bench_minify_and_print,
    bench_prefix,
);
criterion_main!(benches);
