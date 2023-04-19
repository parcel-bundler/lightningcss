use assert_cmd::prelude::*;
use assert_fs::fixture::FixtureError;
use assert_fs::prelude::*;
use indoc::indoc;
use lightningcss::css_modules::CssModuleExport;
use predicates::prelude::*;
use std::collections::HashMap;
use std::fs;
use std::process::Command;

fn test_file() -> Result<assert_fs::NamedTempFile, FixtureError> {
  let file = assert_fs::NamedTempFile::new("test.css")?;
  file.write_str(
    r#"
      .foo {
        border: none;
      }
    "#,
  )?;
  Ok(file)
}

fn test_file2() -> Result<assert_fs::NamedTempFile, FixtureError> {
  let file = assert_fs::NamedTempFile::new("test2.css")?;
  file.write_str(
    r#"
      .foo {
        color: yellow;
      }
    "#,
  )?;
  Ok(file)
}

fn css_module_test_vals() -> (String, String, String) {
  let exports: HashMap<&str, CssModuleExport> = HashMap::from([
    (
      "fade",
      CssModuleExport {
        name: "EgL3uq_fade".into(),
        composes: vec![],
        is_referenced: false,
      },
    ),
    (
      "foo",
      CssModuleExport {
        name: "EgL3uq_foo".into(),
        composes: vec![],
        is_referenced: false,
      },
    ),
    (
      "circles",
      CssModuleExport {
        name: "EgL3uq_circles".into(),
        composes: vec![],
        is_referenced: true,
      },
    ),
    (
      "id",
      CssModuleExport {
        name: "EgL3uq_id".into(),
        composes: vec![],
        is_referenced: false,
      },
    ),
    (
      "test",
      CssModuleExport {
        name: "EgL3uq_test".into(),
        composes: vec![],
        is_referenced: true,
      },
    ),
  ]);
  (
    r#"
      .foo {
        color: red;
      }

      #id {
        animation: 2s test;
      }

      @keyframes test {
        from { color: red }
        to { color: yellow }
      }

      @counter-style circles {
        symbols: Ⓐ Ⓑ Ⓒ;
      }

      ul {
        list-style: circles;
      }

      @keyframes fade {
        from { opacity: 0 }
        to { opacity: 1 }
      }
    "#
    .into(),
    indoc! {r#"
      .EgL3uq_foo {
        color: red;
      }

      #EgL3uq_id {
        animation: 2s EgL3uq_test;
      }

      @keyframes EgL3uq_test {
        from {
          color: red;
        }

        to {
          color: #ff0;
        }
      }

      @counter-style EgL3uq_circles {
        symbols: Ⓐ Ⓑ Ⓒ;
      }

      ul {
        list-style: EgL3uq_circles;
      }

      @keyframes EgL3uq_fade {
        from {
          opacity: 0;
        }

        to {
          opacity: 1;
        }
      }
    "#}
    .into(),
    serde_json::to_string(&exports).unwrap(),
  )
}

#[test]
fn valid_input_file() -> Result<(), Box<dyn std::error::Error>> {
  let file = assert_fs::NamedTempFile::new("test.css")?;
  file.write_str(
    r#"
      .foo {
        border: none;
      }
    "#,
  )?;

  let mut cmd = Command::cargo_bin("lightningcss")?;
  cmd.arg(file.path());
  cmd.assert().success().stdout(predicate::str::contains(indoc! {r#"
        .foo {
          border: none;
        }"#}));

  Ok(())
}

#[test]
fn empty_input_file() -> Result<(), Box<dyn std::error::Error>> {
  let file = assert_fs::NamedTempFile::new("test.css")?;
  file.write_str("")?;

  let mut cmd = Command::cargo_bin("lightningcss")?;
  cmd.arg(file.path());
  cmd.assert().success();

  Ok(())
}

#[test]
fn output_file_option() -> Result<(), Box<dyn std::error::Error>> {
  let infile = test_file()?;
  let outfile = assert_fs::NamedTempFile::new("test.out")?;
  let mut cmd = Command::cargo_bin("lightningcss")?;
  cmd.arg(infile.path());
  cmd.arg("--output-file").arg(outfile.path());
  cmd.assert().success();
  outfile.assert(predicate::str::contains(indoc! {r#"
        .foo {
          border: none;
        }"#}));

  Ok(())
}

#[test]
fn output_file_option_create_missing_directories() -> Result<(), Box<dyn std::error::Error>> {
  let infile = test_file()?;
  let outdir = assert_fs::TempDir::new()?;
  let outfile = outdir.child("out.css");
  outdir.close()?;
  let mut cmd = Command::cargo_bin("lightningcss")?;
  cmd.arg(infile.path());
  cmd.arg("--output-file").arg(outfile.path());
  cmd.assert().success();
  outfile.assert(predicate::str::contains(indoc! {
    r#"
      .foo {
        border: none;
      }
    "#
  }));
  fs::remove_dir_all(outfile.parent().unwrap())?;

  Ok(())
}

#[test]
fn multiple_input_files() -> Result<(), Box<dyn std::error::Error>> {
  let infile = test_file()?;
  let infile2 = test_file2()?;
  let outdir = assert_fs::TempDir::new()?;
  let mut cmd = Command::cargo_bin("lightningcss")?;
  cmd.arg(infile.path());
  cmd.arg(infile2.path());
  cmd.arg("--output-dir").arg(outdir.path());
  cmd.assert().success();
  outdir
    .child(infile.file_name().unwrap())
    .assert(predicate::str::contains(indoc! {r#"
        .foo {
          border: none;
        }"#}));
  outdir
    .child(infile2.file_name().unwrap())
    .assert(predicate::str::contains(indoc! {r#"
        .foo {
          color: #ff0;
        }"#}));

  Ok(())
}

#[test]
fn multiple_input_files_out_file() -> Result<(), Box<dyn std::error::Error>> {
  let infile = test_file()?;
  let infile2 = test_file2()?;
  let outdir = assert_fs::TempDir::new()?;
  let mut cmd = Command::cargo_bin("lightningcss")?;
  cmd.arg(infile.path());
  cmd.arg(infile2.path());
  cmd.arg("--output-file").arg(outdir.path());
  cmd.assert().failure();

  Ok(())
}

#[test]
fn multiple_input_files_stdout() -> Result<(), Box<dyn std::error::Error>> {
  let infile = test_file()?;
  let infile2 = test_file2()?;
  let mut cmd = Command::cargo_bin("lightningcss")?;
  cmd.arg(infile.path());
  cmd.arg(infile2.path());
  cmd.assert().failure();

  Ok(())
}

#[test]
fn minify_option() -> Result<(), Box<dyn std::error::Error>> {
  let infile = test_file()?;
  let mut cmd = Command::cargo_bin("lightningcss")?;
  cmd.arg(infile.path());
  cmd.arg("--minify");
  cmd
    .assert()
    .success()
    .stdout(predicate::str::contains(indoc! {r#".foo{border:none}"#}));

  Ok(())
}

#[test]
fn nesting_option() -> Result<(), Box<dyn std::error::Error>> {
  let infile = assert_fs::NamedTempFile::new("test.css")?;
  infile.write_str(
    r#"
        .foo {
          color: blue;
          & > .bar { color: red; }
        }
      "#,
  )?;

  let mut cmd = Command::cargo_bin("lightningcss")?;
  cmd.arg(infile.path());
  cmd.arg("--targets=defaults");
  cmd.arg("--nesting");
  cmd.assert().success().stdout(predicate::str::contains(indoc! {r#"
        .foo {
          color: #00f;
        }

        .foo > .bar {
          color: red;
        }
      "#}));

  Ok(())
}

#[test]
fn css_modules_infer_output_file() -> Result<(), Box<dyn std::error::Error>> {
  let (input, _, exports) = css_module_test_vals();
  let infile = assert_fs::NamedTempFile::new("test.css")?;
  let outfile = assert_fs::NamedTempFile::new("out.css")?;
  infile.write_str(&input)?;
  let mut cmd = Command::cargo_bin("lightningcss")?;
  cmd.current_dir(infile.path().parent().unwrap());
  cmd.arg(infile.path());
  cmd.arg("--css-modules");
  cmd.arg("-o").arg(outfile.path());
  cmd.assert().success();

  let expected: serde_json::Value = serde_json::from_str(&exports)?;
  let actual: serde_json::Value =
    serde_json::from_str(&std::fs::read_to_string(outfile.path().with_extension("json"))?)?;
  assert_eq!(expected, actual);

  Ok(())
}

#[test]
fn css_modules_output_target_option() -> Result<(), Box<dyn std::error::Error>> {
  let (input, _, exports) = css_module_test_vals();
  let infile = assert_fs::NamedTempFile::new("test.css")?;
  let outfile = assert_fs::NamedTempFile::new("out.css")?;
  let modules_file = assert_fs::NamedTempFile::new("module.json")?;
  infile.write_str(&input)?;
  let mut cmd = Command::cargo_bin("lightningcss")?;
  cmd.current_dir(infile.path().parent().unwrap());
  cmd.arg(infile.path());
  cmd.arg("-o").arg(outfile.path());
  cmd.arg("--css-modules").arg(modules_file.path());
  cmd.assert().success();

  let expected: serde_json::Value = serde_json::from_str(&exports)?;
  let actual: serde_json::Value = serde_json::from_str(&std::fs::read_to_string(modules_file.path())?)?;
  assert_eq!(expected, actual);

  Ok(())
}

#[test]
fn css_modules_stdout() -> Result<(), Box<dyn std::error::Error>> {
  let (input, out_code, exports) = css_module_test_vals();
  let infile = assert_fs::NamedTempFile::new("test.css")?;
  infile.write_str(&input)?;
  let mut cmd = Command::cargo_bin("lightningcss")?;
  cmd.current_dir(infile.path().parent().unwrap());
  cmd.arg(infile.path());
  cmd.arg("--css-modules");
  let assert = cmd.assert().success();
  let output = assert.get_output();

  let expected: serde_json::Value = serde_json::from_str(&exports)?;
  let actual: serde_json::Value = serde_json::from_slice(&output.stdout)?;
  assert_eq!(out_code, actual.pointer("/code").unwrap().as_str().unwrap());
  assert_eq!(&expected, actual.pointer("/exports").unwrap());

  Ok(())
}

#[test]
fn css_modules_pattern() -> Result<(), Box<dyn std::error::Error>> {
  let (input, _, _) = css_module_test_vals();
  let infile = assert_fs::NamedTempFile::new("test.css")?;
  infile.write_str(&input)?;
  let mut cmd = Command::cargo_bin("lightningcss")?;
  cmd.current_dir(infile.path().parent().unwrap());
  cmd.arg(infile.path());
  cmd.arg("--css-modules");
  cmd.arg("--css-modules-pattern").arg("[name]-[hash]-[local]");
  cmd.assert().success().stdout(predicate::str::contains("test-EgL3uq-foo"));

  Ok(())
}

#[test]
fn sourcemap() -> Result<(), Box<dyn std::error::Error>> {
  let (input, _, _) = css_module_test_vals();
  let infile = assert_fs::NamedTempFile::new("test.css")?;
  let outdir = assert_fs::TempDir::new()?;
  let outfile = outdir.child("out.css");
  infile.write_str(&input)?;
  let mut cmd = Command::cargo_bin("lightningcss")?;
  cmd.current_dir(infile.path().parent().unwrap());
  cmd.arg(infile.path());
  cmd.arg("-o").arg(outfile.path());
  cmd.arg("--sourcemap");
  cmd.assert().success();

  outfile.assert(predicate::str::contains(&format!(
    "/*# sourceMappingURL={}.map */",
    outfile.path().to_str().unwrap()
  )));
  let mapfile = outdir.child("out.css.map");
  mapfile.assert(predicate::str::contains(r#""version":3"#));
  mapfile.assert(predicate::str::contains(r#""sources":["test.css"]"#));
  mapfile.assert(predicate::str::contains(
    r#""mappings":"AACM;;;;AAIA;;;;AAIA;;;;;;;;;;AAKA;;;;AAIA;;;;AAIA""#,
  ));

  Ok(())
}

#[test]
fn targets() -> Result<(), Box<dyn std::error::Error>> {
  let file = assert_fs::NamedTempFile::new("test.css")?;
  file.write_str(
    r#"
      @custom-media --foo print;
      @media (--foo) {
        .a { color: red }
      }
    "#,
  )?;

  let mut cmd = Command::cargo_bin("lightningcss")?;
  cmd.arg(file.path());
  cmd.arg("--custom-media");
  cmd.arg("--targets").arg("last 1 Chrome version");
  cmd.assert().success().stdout(predicate::str::contains(indoc! {r#"
        @media print {
          .a {
            color: red;
          }
        }"#}));

  Ok(())
}

#[test]
fn preserve_custom_media() -> Result<(), Box<dyn std::error::Error>> {
  let file = assert_fs::NamedTempFile::new("test.css")?;
  file.write_str(
    r#"
      @custom-media --foo print;
    "#,
  )?;

  let mut cmd = Command::cargo_bin("lightningcss")?;
  cmd.arg(file.path());
  cmd.arg("--custom-media");
  cmd.assert().success().stdout(predicate::str::contains(indoc! {r#"
    @custom-media --foo print;
  "#}));

  Ok(())
}

#[test]
/// Test command line argument parsing failing when `--targets` is used at the same time as `--browserslist`.
/// The two options are mutually exclusive.
fn browserslist_targets_exclusive() -> Result<(), Box<dyn std::error::Error>> {
  let mut cmd = Command::cargo_bin("lightningcss")?;
  cmd.arg("--targets").arg("defaults");
  cmd.arg("--browserslist");
  cmd.assert().failure().stderr(predicate::str::contains(indoc! {r#"
    error: The argument '--targets <TARGETS>' cannot be used with '--browserslist'
  "#}));

  Ok(())
}

#[test]
/// Test browserslist defaults being applied when no configuration is provided or discovered.
///
/// Note: This test might fail in unhygienic environments and should ideally run inside a chroot.
/// We have no control over the contents of our temp dir's parent dir (e.g. `/tmp`).
/// If this parent dir or its ancestors contain a `browserslist`, `.browserslistrc` or `package.json`
/// file, then configuration will be read from there, instead of applying defaults.
fn browserslist_defaults() -> Result<(), Box<dyn std::error::Error>> {
  let dir = assert_fs::TempDir::new()?;
  let file = dir.child("test.css");
  file.write_str(
    r#"
      * {
        -webkit-border-radius: 1rem;
        border-radius: 1rem;
      }
    "#,
  )?;

  let mut cmd = Command::cargo_bin("lightningcss")?;
  cmd.current_dir(dir.path());
  cmd.env_clear();
  cmd.arg("--browserslist");
  cmd.arg(file.path());
  cmd.assert().success().stdout(predicate::str::contains(indoc! {r#"
    * {
      border-radius: 1rem;
    }
  "#}));

  Ok(())
}

#[test]
/// Test browserslist configuration being read from the `BROWSERSLIST` environment variable.
fn browserslist_env_config() -> Result<(), Box<dyn std::error::Error>> {
  let dir = assert_fs::TempDir::new()?;
  let file = dir.child("test.css");
  file.write_str(
    r#"
      * {
        border-radius: 1rem;
      }
    "#,
  )?;

  let mut cmd = Command::cargo_bin("lightningcss")?;
  cmd.current_dir(dir.path());
  cmd.env_clear();
  cmd.env("BROWSERSLIST", "safari 4");
  cmd.arg("--browserslist");
  cmd.arg(file.path());
  cmd.assert().success().stdout(predicate::str::contains(indoc! {r#"
    * {
      -webkit-border-radius: 1rem;
      border-radius: 1rem;
    }
  "#}));

  Ok(())
}

#[test]
/// Test browserslist configuration being read from the file configured
/// by setting the `BROWSERSLIST_CONFIG` environment variable.
fn browserslist_env_config_file() -> Result<(), Box<dyn std::error::Error>> {
  let dir = assert_fs::TempDir::new()?;
  let file = dir.child("test.css");
  file.write_str(
    r#"
      * {
        border-radius: 1rem;
      }
    "#,
  )?;

  let config = dir.child("config");
  config.write_str(
    r#"
      safari 4
    "#,
  )?;

  let mut cmd = Command::cargo_bin("lightningcss")?;
  cmd.current_dir(dir.path());
  cmd.env_clear();
  cmd.env("BROWSERSLIST_CONFIG", config.path());
  cmd.arg("--browserslist");
  cmd.arg(file.path());
  cmd.assert().success().stdout(predicate::str::contains(indoc! {r#"
    * {
      -webkit-border-radius: 1rem;
      border-radius: 1rem;
    }
  "#}));

  Ok(())
}

#[test]
/// Test `browserslist` configuration file being read.
fn browserslist_config_discovery() -> Result<(), Box<dyn std::error::Error>> {
  let dir = assert_fs::TempDir::new()?;
  let file = dir.child("test.css");
  file.write_str(
    r#"
      * {
        border-radius: 1rem;
      }
    "#,
  )?;

  let config = dir.child("browserslist");
  config.write_str(
    r#"
      safari 4
    "#,
  )?;

  let mut cmd = Command::cargo_bin("lightningcss")?;
  cmd.current_dir(dir.path());
  cmd.env_clear();
  cmd.arg("--browserslist");
  cmd.arg(file.path());
  cmd.assert().success().stdout(predicate::str::contains(indoc! {r#"
    * {
      -webkit-border-radius: 1rem;
      border-radius: 1rem;
    }
  "#}));

  Ok(())
}

#[test]
/// Test `.browserslistrc` configuration file being read.
fn browserslist_rc_discovery() -> Result<(), Box<dyn std::error::Error>> {
  let dir = assert_fs::TempDir::new()?;
  let file = dir.child("test.css");
  file.write_str(
    r#"
      * {
        border-radius: 1rem;
      }
    "#,
  )?;

  let config = dir.child(".browserslistrc");
  config.write_str(
    r#"
      safari 4
    "#,
  )?;

  let mut cmd = Command::cargo_bin("lightningcss")?;
  cmd.current_dir(dir.path());
  cmd.env_clear();
  cmd.arg("--browserslist");
  cmd.arg(file.path());
  cmd.assert().success().stdout(predicate::str::contains(indoc! {r#"
    * {
      -webkit-border-radius: 1rem;
      border-radius: 1rem;
    }
  "#}));

  Ok(())
}

#[test]
/// Test `package.json` configuration section being read.
fn browserslist_package_discovery() -> Result<(), Box<dyn std::error::Error>> {
  let dir = assert_fs::TempDir::new()?;
  let file = dir.child("test.css");
  file.write_str(
    r#"
      * {
        border-radius: 1rem;
      }
    "#,
  )?;

  let config = dir.child("package.json");
  config.write_str(
    r#"
      {
        "browserslist": "safari 4"
      }
    "#,
  )?;

  let mut cmd = Command::cargo_bin("lightningcss")?;
  cmd.current_dir(dir.path());
  cmd.env_clear();
  cmd.arg("--browserslist");
  cmd.arg(file.path());
  cmd.assert().success().stdout(predicate::str::contains(indoc! {r#"
    * {
      -webkit-border-radius: 1rem;
      border-radius: 1rem;
    }
  "#}));

  Ok(())
}

#[test]
/// Test environment targets being applied from the `NODE_ENV` environment variable.
fn browserslist_environment_from_node_env() -> Result<(), Box<dyn std::error::Error>> {
  let dir = assert_fs::TempDir::new()?;
  let file = dir.child("test.css");
  file.write_str(
    r#"
      * {
        border-radius: 1rem;
      }
    "#,
  )?;

  let config = dir.child("browserslist");
  config.write_str(
    r#"
      last 1 Chrome version

      [legacy]
      safari 4
    "#,
  )?;

  let mut cmd = Command::cargo_bin("lightningcss")?;
  cmd.current_dir(dir.path());
  cmd.env_clear();
  cmd.env("NODE_ENV", "legacy");
  cmd.arg("--browserslist");
  cmd.arg(file.path());
  cmd.assert().success().stdout(predicate::str::contains(indoc! {r#"
    * {
      -webkit-border-radius: 1rem;
      border-radius: 1rem;
    }
  "#}));

  Ok(())
}

#[test]
/// Test environment targets being applied from the `BROWSERSLIST_ENV` environment variable.
fn browserslist_environment_from_browserslist_env() -> Result<(), Box<dyn std::error::Error>> {
  let dir = assert_fs::TempDir::new()?;
  let file = dir.child("test.css");
  file.write_str(
    r#"
      * {
        border-radius: 1rem;
      }
    "#,
  )?;

  let config = dir.child("browserslist");
  config.write_str(
    r#"
      last 1 Chrome version

      [legacy]
      safari 4
    "#,
  )?;

  let mut cmd = Command::cargo_bin("lightningcss")?;
  cmd.current_dir(dir.path());
  cmd.env_clear();
  cmd.env("BROWSERSLIST_ENV", "legacy");
  cmd.arg("--browserslist");
  cmd.arg(file.path());
  cmd.assert().success().stdout(predicate::str::contains(indoc! {r#"
    * {
      -webkit-border-radius: 1rem;
      border-radius: 1rem;
    }
  "#}));

  Ok(())
}
