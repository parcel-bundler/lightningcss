mod common;

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};

fn bench_stylesheet_parse(c: &mut Criterion) {
  let fixtures = common::stylesheet_fixtures();
  let mut group = c.benchmark_group("stylesheet/parse");

  for fixture in &fixtures {
    group.throughput(Throughput::Bytes(fixture.css.len() as u64));
    group.bench_with_input(
      BenchmarkId::from_parameter(fixture.name),
      fixture.css.as_str(),
      |b, css| {
        b.iter(|| common::parse_stylesheet(black_box(css)));
      },
    );
  }

  group.finish();
}

fn bench_stylesheet_transform(c: &mut Criterion) {
  let fixtures = common::stylesheet_fixtures();
  let mut group = c.benchmark_group("stylesheet/transform");

  for fixture in &fixtures {
    group.throughput(Throughput::Bytes(fixture.css.len() as u64));
    group.bench_with_input(
      BenchmarkId::from_parameter(fixture.name),
      fixture.css.as_str(),
      |b, css| {
        b.iter(|| common::transform_stylesheet(black_box(css)));
      },
    );
  }

  group.finish();
}

criterion_group!(benches, bench_stylesheet_parse, bench_stylesheet_transform);
criterion_main!(benches);
