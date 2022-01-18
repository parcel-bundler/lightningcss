use assert_cmd::prelude::*;
use predicates::prelude::*;
use std::process::Command;
use assert_fs::prelude::*;
use indoc::indoc;

#[test]
fn valid_input_file() -> Result<(), Box<dyn std::error::Error>> {
    let file = assert_fs::NamedTempFile::new("test.css")?;
    file.write_str(r#"
      .foo {
        border: none;
      }
    "#)?;


    let mut cmd = Command::cargo_bin("parcel_css")?;
    cmd.arg(file.path());
    cmd.assert()
        .success()
        .stdout(predicate::str::contains(indoc! {r#"
        .foo {
          border: none;
        }"#}));

    Ok(())
}

#[test]
fn no_input_file() -> Result<(), Box<dyn std::error::Error>> {
    panic!("todo test");
}

#[test]
fn empty_input_file() -> Result<(), Box<dyn std::error::Error>> {
    panic!("todo test");
}

#[test]
fn output_file_option() -> Result<(), Box<dyn std::error::Error>> {
    panic!("todo test");
}

#[test]
fn minify_option() -> Result<(), Box<dyn std::error::Error>> {
    panic!("todo test");
}

#[test]
fn nesting_option() -> Result<(), Box<dyn std::error::Error>> {
    panic!("todo test");
}

#[test]
fn css_modules_stdout() -> Result<(), Box<dyn std::error::Error>> {
    panic!("todo test");
}

#[test]
fn css_modules_infer_output_file() -> Result<(), Box<dyn std::error::Error>> {
    panic!("todo test");
}

#[test]
fn css_modules_output_target_option() -> Result<(), Box<dyn std::error::Error>> {
    panic!("todo test");
}
